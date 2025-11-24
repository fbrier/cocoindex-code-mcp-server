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
Repository management for cloning and indexing git repositories.
"""

import hashlib
import json
import logging
import os
import re
import subprocess
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, Optional
from urllib.parse import urlparse

logger = logging.getLogger(__name__)


class RepositoryManager:
    """Manages git repository cloning and code fragment storage."""

    def __init__(self, repos_dir: str = "/repos", fragments_dir: str = "/code_fragments"):
        """
        Initialize the repository manager.

        Args:
            repos_dir: Directory where git repositories are cloned
            fragments_dir: Directory where code fragments are stored
        """
        self.repos_dir = Path(repos_dir)
        self.fragments_dir = Path(fragments_dir)

        # Create directories if they don't exist
        self.repos_dir.mkdir(parents=True, exist_ok=True)
        self.fragments_dir.mkdir(parents=True, exist_ok=True)

        logger.info(f"Repository manager initialized: repos={self.repos_dir}, fragments={self.fragments_dir}")

    def _get_ssh_key_path(self) -> Optional[Path]:
        """Get SSH private key path from environment or default location."""
        ssh_key_env = os.getenv("GIT_SSH_KEY")
        if ssh_key_env:
            return Path(ssh_key_env)

        # Check default location
        default_key = Path.home() / ".ssh" / "id_rsa"
        if default_key.exists():
            return default_key

        return None

    def _extract_repo_name(self, git_url: str) -> str:
        """
        Extract repository name from git URL.

        Examples:
            https://github.com/user/repo.git -> repo
            git@github.com:user/repo.git -> repo
            https://github.com/user/repo -> repo
        """
        # Remove .git suffix if present
        url = git_url.rstrip("/")
        if url.endswith(".git"):
            url = url[:-4]

        # Extract the last part of the path
        parts = url.split("/")
        repo_name = parts[-1]

        # For git@ URLs, handle the user/repo part
        if ":" in repo_name:
            repo_name = repo_name.split(":")[-1].split("/")[-1]

        return repo_name

    def _setup_git_ssh(self) -> Optional[Dict[str, str]]:
        """
        Setup Git SSH environment for private repository access.

        Returns:
            Environment variables dict for subprocess, or None if no SSH key
        """
        ssh_key_path = self._get_ssh_key_path()
        if not ssh_key_path:
            return None

        # Create GIT_SSH_COMMAND environment variable
        git_ssh_cmd = f'ssh -i {ssh_key_path} -o StrictHostKeyChecking=no'
        env = os.environ.copy()
        env["GIT_SSH_COMMAND"] = git_ssh_cmd

        logger.info(f"Using SSH key: {ssh_key_path}")
        return env

    def clone_repository(
        self,
        git_url: str,
        branch: Optional[str] = None,
        update_existing: bool = True,
        subdirectory: Optional[str] = None,
    ) -> Dict[str, Any]:
        """
        Clone a git repository and prepare it for indexing.

        Args:
            git_url: Git repository URL
            branch: Branch to clone (None for default branch)
            update_existing: If True, pull existing repo instead of failing
            subdirectory: Optional subdirectory to index within the repo

        Returns:
            Dict with status, repo_path, and other metadata
        """
        try:
            repo_name = self._extract_repo_name(git_url)
            repo_path = self.repos_dir / repo_name

            logger.info(f"Processing repository: {repo_name} from {git_url}")

            # Setup SSH if needed
            env = self._setup_git_ssh()

            # Check if repository already exists
            if repo_path.exists():
                if not update_existing:
                    return {
                        "status": "error",
                        "message": f"Repository {repo_name} already exists. Use update_existing=true to pull latest changes.",
                        "repo_path": str(repo_path),
                        "repo_name": repo_name,
                    }

                logger.info(f"Updating existing repository: {repo_name}")
                try:
                    # Git pull
                    cmd = ["git", "-C", str(repo_path), "pull"]
                    subprocess.run(cmd, check=True, capture_output=True, text=True, env=env)

                    return {
                        "status": "updated",
                        "message": f"Repository {repo_name} updated successfully",
                        "repo_path": str(repo_path),
                        "repo_name": repo_name,
                    }
                except subprocess.CalledProcessError as e:
                    logger.error(f"Failed to update repository: {e.stderr}")
                    return {
                        "status": "error",
                        "message": f"Failed to update repository: {e.stderr}",
                        "repo_path": str(repo_path),
                        "repo_name": repo_name,
                    }

            # Clone the repository
            logger.info(f"Cloning repository: {git_url} -> {repo_path}")
            cmd = ["git", "clone"]

            if branch:
                cmd.extend(["--branch", branch])

            cmd.extend([git_url, str(repo_path)])

            try:
                result = subprocess.run(cmd, check=True, capture_output=True, text=True, env=env)
                logger.info(f"Repository cloned successfully: {repo_name}")

                # Get path to index (full repo or subdirectory)
                index_path = repo_path
                if subdirectory:
                    index_path = repo_path / subdirectory
                    if not index_path.exists():
                        return {
                            "status": "error",
                            "message": f"Subdirectory '{subdirectory}' not found in repository",
                            "repo_path": str(repo_path),
                            "repo_name": repo_name,
                        }

                return {
                    "status": "success",
                    "message": f"Repository {repo_name} cloned successfully",
                    "repo_path": str(index_path),
                    "repo_name": repo_name,
                }

            except subprocess.CalledProcessError as e:
                logger.error(f"Failed to clone repository: {e.stderr}")
                return {
                    "status": "error",
                    "message": f"Failed to clone repository: {e.stderr}",
                    "repo_path": "",
                    "repo_name": repo_name,
                }

        except Exception as e:
            logger.exception(f"Unexpected error cloning repository: {e}")
            return {
                "status": "error",
                "message": f"Unexpected error: {str(e)}",
                "repo_path": "",
                "repo_name": "",
            }

    def _url_to_filename(self, url: str) -> str:
        """
        Convert URL to a safe filename using hash.

        Args:
            url: Source URL

        Returns:
            Safe filename based on URL hash
        """
        # Create hash of URL for uniqueness
        url_hash = hashlib.md5(url.encode()).hexdigest()[:12]

        # Extract domain for context
        try:
            parsed = urlparse(url)
            domain = parsed.netloc.replace("www.", "").split(".")[0]
        except:
            domain = "web"

        return f"{domain}_{url_hash}"

    def ingest_code_fragment(
        self,
        source_url: str,
        code: str,
        language: Optional[str] = None,
        function_name: Optional[str] = None,
        context_tags: Optional[list[str]] = None,
        additional_metadata: Optional[Dict[str, Any]] = None,
        extraction_date: Optional[str] = None,
    ) -> Dict[str, Any]:
        """
        Store a code fragment from a webpage and prepare it for indexing.

        Args:
            source_url: URL where the code fragment was found
            code: The code content
            language: Programming language (auto-detected if not provided)
            function_name: Function/method name in the code
            context_tags: Contextual tags like ['dependency-injection', 'logging']
            additional_metadata: Additional metadata dict
            extraction_date: ISO 8601 date (auto-set if not provided)

        Returns:
            Dict with status, file_path, fragment_id, and metadata
        """
        try:
            # Auto-detect language if not provided
            if not language:
                language = self._detect_language(code)

            # Set extraction date if not provided
            if not extraction_date:
                extraction_date = datetime.utcnow().isoformat() + "Z"

            # Generate unique fragment ID
            fragment_id = hashlib.sha256(f"{source_url}:{code}".encode()).hexdigest()[:16]

            # Create filename
            base_filename = self._url_to_filename(source_url)
            if function_name:
                # Sanitize function name for filename
                safe_func_name = re.sub(r'[^\w\-]', '_', function_name)
                base_filename = f"{base_filename}_{safe_func_name}"

            # Add language extension
            ext = self._get_file_extension(language)
            filename = f"{base_filename}_{fragment_id}{ext}"

            # Full file path
            file_path = self.fragments_dir / filename

            # Prepare metadata
            metadata = {
                "fragment_id": fragment_id,
                "source_url": source_url,
                "language": language,
                "extraction_date": extraction_date,
                "function_name": function_name,
                "context_tags": context_tags or [],
                "additional_metadata": additional_metadata or {},
            }

            # Write code file
            file_path.write_text(code, encoding="utf-8")

            # Write metadata as JSON sidecar
            metadata_path = file_path.with_suffix(file_path.suffix + ".meta.json")
            metadata_path.write_text(json.dumps(metadata, indent=2), encoding="utf-8")

            logger.info(f"Code fragment saved: {filename} ({language})")

            return {
                "status": "success",
                "message": f"Code fragment saved successfully",
                "file_path": str(file_path),
                "fragment_id": fragment_id,
                "detected_language": language,
                "indexed": False,  # Will be set to True after indexing
                "metadata": metadata,
            }

        except Exception as e:
            logger.exception(f"Error ingesting code fragment: {e}")
            return {
                "status": "error",
                "message": f"Failed to ingest code fragment: {str(e)}",
                "file_path": "",
                "fragment_id": "",
                "detected_language": language or "unknown",
                "indexed": False,
                "metadata": {},
            }

    def _detect_language(self, code: str) -> str:
        """
        Simple language detection based on code patterns.

        Args:
            code: Code content

        Returns:
            Detected language name
        """
        # Simple heuristic-based detection
        code_lower = code.lower()

        if "namespace" in code_lower and ("class" in code_lower or "public" in code_lower):
            return "csharp"
        elif "def " in code and ":" in code:
            return "python"
        elif "#include" in code or "std::" in code:
            return "cpp"
        elif "function" in code_lower or "const " in code or "let " in code:
            if "=>" in code:
                return "javascript"
            return "javascript"
        elif "interface" in code_lower and "{" in code:
            return "typescript"
        elif "public class" in code or "public static" in code:
            return "java"
        elif "fn " in code and "->" in code:
            return "rust"
        elif "func " in code and "{" in code:
            return "go"
        else:
            return "plaintext"

    def _get_file_extension(self, language: str) -> str:
        """Get file extension for a programming language."""
        extensions = {
            "csharp": ".cs",
            "python": ".py",
            "cpp": ".cpp",
            "c": ".c",
            "javascript": ".js",
            "typescript": ".ts",
            "java": ".java",
            "rust": ".rs",
            "go": ".go",
            "ruby": ".rb",
            "php": ".php",
            "swift": ".swift",
            "kotlin": ".kt",
            "scala": ".scala",
            "haskell": ".hs",
            "plaintext": ".txt",
        }
        return extensions.get(language.lower(), ".txt")
