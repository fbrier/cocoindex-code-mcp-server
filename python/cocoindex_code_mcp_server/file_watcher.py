#!/usr/bin/env python3
#
# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 aanno <aanno@users.noreply.github.com>
#
# This file is part of cocoindex_code_mcp_server from
# https://github.com/aanno/cocoindex-code-mcp-server
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

"""
File system watcher using watchdog library (inotify wrapper).

Monitors directories for file changes and triggers re-indexing when:
- New files are created (e.g., code fragments from crawl4ai)
- Existing files are modified
- Files are deleted

This replaces the inefficient polling mechanism with OS-level event notifications.
"""

import logging
import os
import threading
import time
from pathlib import Path
from typing import Callable, List, Set

from watchdog.events import FileSystemEvent, FileSystemEventHandler
from watchdog.observers import Observer

logger = logging.getLogger(__name__)


class CodeFileEventHandler(FileSystemEventHandler):
    """
    File system event handler for code files.

    Filters events to only trigger on code files (not .git, __pycache__, etc.)
    and debounces rapid successive events.
    """

    # File extensions to monitor (code files only)
    CODE_EXTENSIONS = {
        ".py", ".js", ".ts", ".tsx", ".jsx", ".java", ".cs", ".cpp", ".cc", ".cxx", ".c", ".h", ".hpp",
        ".rs", ".go", ".kt", ".kts", ".swift", ".rb", ".php", ".scala", ".r", ".m", ".mm", ".sh",
        ".bash", ".zsh", ".ps1", ".sql", ".proto", ".thrift", ".graphql", ".html", ".css", ".scss",
        ".less", ".vue", ".dart", ".lua", ".perl", ".pl", ".hs", ".clj", ".ex", ".exs", ".erl",
        ".ml", ".fs", ".fsx", ".vb", ".bas", ".f90", ".f95", ".f03", ".jl", ".groovy", ".gradle",
    }

    # Directories to ignore
    IGNORE_DIRS = {
        ".git", ".svn", ".hg", "__pycache__", ".pytest_cache", "node_modules", ".venv", "venv",
        ".tox", ".mypy_cache", ".idea", ".vscode", "dist", "build", ".godot", "coverage",
        ".coverage", "htmlcov", ".eggs", "*.egg-info",
    }

    def __init__(self, on_change_callback: Callable[[], None], debounce_seconds: float = 2.0) -> None:
        """
        Initialize event handler.

        Args:
            on_change_callback: Function to call when relevant file changes detected
            debounce_seconds: Minimum time between triggering re-index (prevents rapid triggers)
        """
        super().__init__()
        self.on_change_callback = on_change_callback
        self.debounce_seconds = debounce_seconds
        self._last_trigger_time = 0.0
        self._pending_events: Set[str] = set()
        self._debounce_timer: threading.Timer | None = None
        self._lock = threading.Lock()

    def _is_code_file(self, file_path: str) -> bool:
        """Check if file is a code file we should monitor."""
        path = Path(file_path)

        # Ignore directories
        if path.is_dir():
            return False

        # Check if in ignored directory
        for part in path.parts:
            if part in self.IGNORE_DIRS or part.startswith("."):
                return False

        # Check file extension
        suffix = path.suffix.lower()
        if suffix in self.CODE_EXTENSIONS:
            return True

        # Also accept .meta.json files (code fragment metadata)
        if file_path.endswith(".meta.json"):
            return True

        return False

    def _should_trigger(self, event: FileSystemEvent) -> bool:
        """Determine if event should trigger re-indexing."""
        # Ignore directory events
        if event.is_directory:
            return False

        # Check if it's a code file
        if not self._is_code_file(event.src_path):
            return False

        return True

    def _trigger_reindex(self) -> None:
        """Trigger re-indexing after debounce delay."""
        with self._lock:
            current_time = time.time()
            time_since_last = current_time - self._last_trigger_time

            if time_since_last >= self.debounce_seconds:
                # Enough time has passed, trigger now
                self._do_trigger()
            else:
                # Schedule trigger for later
                if self._debounce_timer is not None:
                    self._debounce_timer.cancel()

                delay = self.debounce_seconds - time_since_last
                self._debounce_timer = threading.Timer(delay, self._do_trigger)
                self._debounce_timer.daemon = True
                self._debounce_timer.start()

    def _do_trigger(self) -> None:
        """Actually trigger the re-index callback."""
        with self._lock:
            if self._pending_events:
                logger.info("🔄 File changes detected (%d files), triggering re-index...", len(self._pending_events))
                logger.debug("Changed files: %s", ", ".join(sorted(self._pending_events)[:10]))

                try:
                    self.on_change_callback()
                    self._last_trigger_time = time.time()
                    self._pending_events.clear()
                except Exception as e:
                    logger.error("❌ Error during re-index callback: %s", e, exc_info=True)

    def on_created(self, event: FileSystemEvent) -> None:
        """Handle file creation events."""
        if self._should_trigger(event):
            logger.debug("📝 File created: %s", event.src_path)
            with self._lock:
                self._pending_events.add(event.src_path)
            self._trigger_reindex()

    def on_modified(self, event: FileSystemEvent) -> None:
        """Handle file modification events."""
        if self._should_trigger(event):
            logger.debug("✏️  File modified: %s", event.src_path)
            with self._lock:
                self._pending_events.add(event.src_path)
            self._trigger_reindex()

    def on_deleted(self, event: FileSystemEvent) -> None:
        """Handle file deletion events."""
        if self._should_trigger(event):
            logger.debug("🗑️  File deleted: %s", event.src_path)
            with self._lock:
                self._pending_events.add(event.src_path)
            self._trigger_reindex()


class FileWatcher:
    """
    File system watcher using watchdog (inotify wrapper).

    Monitors multiple directories for code file changes and triggers re-indexing.
    """

    def __init__(
        self, watch_paths: List[str], on_change_callback: Callable[[], None], debounce_seconds: float = 2.0
    ) -> None:
        """
        Initialize file watcher.

        Args:
            watch_paths: List of directory paths to monitor
            on_change_callback: Function to call when file changes detected
            debounce_seconds: Minimum time between re-index triggers
        """
        self.watch_paths = watch_paths
        self.on_change_callback = on_change_callback
        self.debounce_seconds = debounce_seconds

        self.observer = Observer()
        self.event_handler = CodeFileEventHandler(on_change_callback, debounce_seconds)
        self._started = False

    def start(self) -> None:
        """Start monitoring file system."""
        if self._started:
            logger.warning("File watcher already started")
            return

        logger.info("👁️  Starting file system watcher (inotify)...")

        for path in self.watch_paths:
            if not os.path.exists(path):
                logger.warning("⚠️  Watch path does not exist: %s", path)
                continue

            logger.info("  Watching: %s", path)
            self.observer.schedule(self.event_handler, path, recursive=True)

        self.observer.start()
        self._started = True
        logger.info("✅ File watcher started successfully")

    def stop(self) -> None:
        """Stop monitoring file system."""
        if not self._started:
            return

        logger.info("⏹️  Stopping file watcher...")
        self.observer.stop()
        self.observer.join(timeout=5.0)
        self._started = False
        logger.info("✅ File watcher stopped")

    def is_alive(self) -> bool:
        """Check if observer thread is running."""
        return self._started and self.observer.is_alive()
