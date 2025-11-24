# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Model Context Protocol (MCP) server providing RAG (Retrieval Augmented Generation) capabilities specialized for code retrieval. It combines CocoIndex (a hybrid Rust/Python data transformation framework) with language-specific code analysis, tree-sitter parsing, and smart embeddings.

Key capabilities:
- Hybrid search combining vector similarity and keyword metadata search
- Smart embedding with language-specific models (GraphCodeBERT, UniXcoder)
- Tree-sitter based AST chunking for 20+ programming languages
- Incremental indexing with live update mode
- PostgreSQL + pgvector backend

## Running the Code

### Main Entry Points

```bash
# Start MCP server (main entry point)
python -m cocoindex_code_mcp_server.main_mcp_server --rescan --port 3033 <path_to_code>

# Interactive query mode
python -m cocoindex_code_mcp_server.main_interactive_query <path_to_code>

# Hybrid search testing
python -m cocoindex_code_mcp_server.main_hybrid_search
```

### Installation and Setup

```bash
# Install from PyPI
pip install cocoindex-code-mcp-server

# Or build from source
uv sync --all-extras
maturin develop

# Start PostgreSQL + pgvector database
./scripts/cocoindex-postgresql.sh
./scripts/install-pgvector.py

# Configure database connection (.env file)
cp .env.template .env
# Edit COCOINDEX_DATABASE_URL if needed
```

## Testing

Tests are located in `./tests/` (NOT in `./python/cocoindex_code_mcp_server/tests/`).

```bash
# Run all tests
pytest -c pytest.ini tests/

# Run specific test file
pytest -c pytest.ini tests/test_hybrid_search_integration.py

# Run with coverage
./scripts/pytest-with-coverage.sh

# Run hybrid search tests with coverage
./scripts/hybrid-search-with-coverage.sh
```

**Important testing guidelines:**
- Use pytest only (not unittest)
- Tests use markers defined in pytest.ini for categorization
- Database integration tests require running PostgreSQL instance
- MCP integration tests marked with `mcp_integration`

## Code Quality and Type Checking

This codebase is mypy compatible and uses multiple code quality tools:

```bash
# Type checking
./scripts/mypy-check.sh           # Check main source
./scripts/mypy-check-tests.sh     # Check tests

# Code formatting
./scripts/format-python.sh python/cocoindex_code_mcp_server/

# Pre-commit hooks (runs isort, yaml checks, json formatting, etc.)
pre-commit install
pre-commit run --all-files
```

Formatting tools used: isort, ruff, autoflake8, autopep8, pydocstyle

## Development Guidelines

- If you are technically stuck or unsure about the next step, ask for help.
- CocoIndex is complex - don't hesitate to ask for clarification or guidance.
- You MUST use our own RAG (MCP server 'cocoindex-rag') before using grep or search.
- Never try to start/stop our RAG MCP server, just ask, I will do it for you.
- Use gw-memory to store and retrieve information about the codebase.
  + Tag all entries with 'cocoindex_code_mcp_server' to indicate they are related to the code MCP server.
  + After you have been started, retrieve what's been stored lately for latest context.

## MCP Tool Usage

- Don't use 'vscode-mcp-server - execute_shell_command_code (MCP)' - use bash directly instead.
- For editing files, prefer 'Opened changes in Visual Studio Code' over 'update'.
- If using 'update', always use 'filesystem - read_text_file (MCP)' first to read the file.

## Database Configuration

DB connection properties are in `.env` file and loaded with `load_dotenv`.
See examples throughout the codebase, especially in `python/cocoindex_code_mcp_server/db/` modules.

## Architecture

This project is a **hybrid Rust/Python MCP server** for code RAG. Key architectural components:

### Python Package Structure (`python/cocoindex_code_mcp_server/`)

**Main Entry Points:**
- `main_mcp_server.py`: MCP server with HTTP streaming endpoint
- `main_interactive_query.py`: Interactive query mode for testing
- `main_hybrid_search.py`: Hybrid search engine (vector + keyword)

**Core Modules:**
- `cocoindex_config.py`: CocoIndex flow configuration and dataflow definitions
- `smart_code_embedding.py`: Language-aware embedding model selection (GraphCodeBERT, UniXcoder)
- `mappers.py`: Language and file extension mappings
- `ast_chunking.py`: AST-based code chunking logic
- `ast_visitor.py`: Generic tree-sitter AST visitor framework
- `keyword_search_parser_lark.py`: Lark-based keyword query parser
- `query_abstraction.py`: Database query abstraction layer

**Subdirectories:**
- `db/`: Database backend implementations (PostgreSQL + pgvector)
- `lang/`: Language-specific analyzers (Python, Haskell)
- `language_handlers/`: Tree-sitter visitors for 20+ languages (Rust, Java, TypeScript, Kotlin, C, C++, etc.)
- `backends/`: Vector database backend abstractions
- `grammars/`: Lark grammar definitions for query parsing

### Rust Components (`rust/`)

- `src/lib.rs`: Haskell tree-sitter extension built with maturin/PyO3
- Compiled as `_haskell_tree_sitter` Python extension module
- Provides Haskell AST parsing capabilities via tree-sitter-haskell

### Data Flow

1. **Indexing Pipeline**:
   - Files → Language Detection (by extension) → Tree-sitter Parsing → AST Chunking → Smart Embedding Selection → Vector DB Storage

2. **Query Pipeline**:
   - User Query → Embedding → Hybrid Search (Vector Similarity + Keyword Metadata) → Ranked Results

3. **Live Updates**:
   - File System Watcher → Changed Files Detection → Incremental Re-indexing → CocoIndex Flow Updates

### Key CocoIndex Concepts

**Dataflow Programming Model:**
- Transformations defined as dataflows using Python decorators (`@cocoindex.flow_def`)
- Each transformation creates new fields without mutation
- System tracks data lineage for incremental processing

**Incremental Processing:**
- Core engine tracks dependencies and only recomputes changed portions
- Uses fingerprinting and memoization for efficiency
- Supports live updates with minimal recomputation

See [Code-Structure](docs/claude/Code-Structure.md) for detailed component documentation.

**Reference Submodules** (for understanding, not direct dependencies):
- `cocoindex/`: CocoIndex framework source (installed from PyPI)
- `astchunk/`: ASTChunk library for advanced chunking (git submodule)
- `code-index-mcp/`: Related MCP server project

## Build and Development Workflow

### Building from Source

```bash
# Install Python dependencies
uv sync --all-extras

# Build Rust extension and install Python package
maturin develop

# (Optional) Build and test Rust components separately
cd rust && cargo build
cargo test
```

### Making Changes

1. **Rust Changes**: After modifying Rust code, run `maturin develop` to rebuild the extension
2. **Python Changes**: Python code changes are immediately available (no rebuild needed)
3. **Testing**: Run `pytest tests/` after changes
4. **Pre-commit**: Run pre-commit hooks before committing

### Code Formatting

```bash
# Format Python code (runs isort, ruff, autoflake8, autopep8, pydocstyle)
./scripts/format-python.sh python/

# Or use ruff directly
ruff format python/
ruff check --fix python/

# Format Rust code (if modifying Rust)
cd rust && cargo fmt
```

## Key Configuration Files

- **`pyproject.toml`**: Python package config, dependencies, tool settings (mypy, ruff, etc.)
- **`rust/Cargo.toml`**: Rust dependencies and build configuration
- **`.pre-commit-config.yaml`**: Pre-commit hook configuration
- **`pytest.ini`**: Pytest configuration and test markers
- **`ruff.toml`**: Ruff linting configuration
- **`.env`**: Database connection strings (copy from `.env.template`)

## Requirements

- **Python**: 3.11+ (supports 3.11, 3.12, 3.13, 3.14)
- **Rust**: Latest stable (for building Rust extension)
- **Maturin**: For building Python extensions from Rust
- **PostgreSQL**: With pgvector extension
- **Tree-sitter**: Language parsers (auto-installed via pyproject.toml)
- **Git**: Required for repository cloning feature

## New MCP Tools

### Repository and Code Fragment Management

The server now includes two additional MCP tools for ingesting code from external sources:

#### `clone-and-index-repo`
Clone a git repository and automatically index it for code search.

**Features:**
- Public repository support (HTTPS)
- Private repository support (SSH with key authentication via `GIT_SSH_KEY` environment variable)
- Branch selection
- Update existing repositories (git pull)
- Subdirectory indexing

**Usage Example:**
```json
{
  "name": "clone-and-index-repo",
  "arguments": {
    "git_url": "https://github.com/microsoft/semantic-kernel.git",
    "branch": "main",
    "update_existing": true
  }
}
```

**Arguments:**
- `git_url` (required): Git repository URL (https:// or git@)
- `branch` (optional): Branch to clone (defaults to repository default)
- `update_existing` (optional, default: true): If true, pulls updates for existing repos
- `subdirectory` (optional): Specific subdirectory to index within the repo

#### `ingest-code-fragment`
Store and index code fragments from webpages with metadata.

**Features:**
- Auto-detects programming language (C#, Python, C++, JavaScript, TypeScript, etc.)
- Creates unique filenames based on URL hash
- Stores metadata as JSON sidecar files
- Supports context tags (e.g., "dependency-injection", "logging")
- Custom metadata support

**Usage Example:**
```json
{
  "name": "ingest-code-fragment",
  "arguments": {
    "source_url": "https://learn.microsoft.com/en-us/dotnet/csharp/...",
    "code": "public class MyService { ... }",
    "language": "csharp",
    "function_name": "MyService",
    "context_tags": ["dependency-injection", "dotnet"],
    "additional_metadata": {
      "framework": "ASP.NET Core",
      "version": "8.0"
    }
  }
}
```

**Arguments:**
- `source_url` (required): URL where the code was found
- `code` (required): The code content
- `language` (optional): Programming language (auto-detected if not provided)
- `function_name` (optional): Function/method name (used in filename)
- `context_tags` (optional): Array of contextual tags
- `additional_metadata` (optional): Custom metadata object
- `extraction_date` (optional): ISO 8601 date (auto-set if not provided)

**Data Storage:**
- Repositories: Stored in `REPOS_DIR` (default: `/repos`)
- Code fragments: Stored in `CODE_FRAGMENTS_DIR` (default: `/code_fragments`)
- Metadata: Stored as `.meta.json` sidecar files alongside code fragments

**Integration with Crawl4AI:**
These tools are designed to be called by Crawl4AI when it detects code fragments during web crawling. Crawl4AI hooks can invoke `ingest-code-fragment` automatically.
