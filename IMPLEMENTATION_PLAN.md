# Implementation Plan: Git Cloning and Code Fragment Ingestion

## Summary

This document outlines the changes needed to add two new MCP tools to the cocoindex-code-mcp-server:
1. `clone-and-index-repo`: Clone git repositories and index them
2. `ingest-code-fragment`: Store and index code fragments from webpages

## Completed Work

✅ Created `repository_manager.py` with core functionality
✅ Designed MCP tool schemas
✅ Implementation logic for git cloning with SSH key support
✅ Implementation logic for code fragment storage with metadata

## Remaining Changes

### 1. Add Schemas to `mcp_json_schemas.py`

Add the following schemas at the end of the file (after `EMPTY_JSON_SCHEMA`):

```python
# New schemas for repository cloning and code fragment ingestion
CLONE_AND_INDEX_REPO_INPUT_SCHEMA = {
    "$schema": "http://json-schema.org/draft-07/schema#",
    "type": "object",
    "properties": {
        "git_url": {
            "type": "string",
            "description": "Git repository URL (https:// or git@). Supports both public and private repositories.",
        },
        "branch": {
            "type": "string",
            "description": "Branch to clone (default: main or master). If not specified, will use repository default branch.",
        },
        "update_existing": {
            "type": "boolean",
            "description": "If true, performs git pull on existing repository instead of failing. Default: true",
            "default": True,
        },
        "subdirectory": {
            "type": "string",
            "description": "Optional subdirectory within the repo to index (indexes entire repo if not specified)",
        },
    },
    "required": ["git_url"],
}

INGEST_CODE_FRAGMENT_INPUT_SCHEMA = {
    "$schema": "http://json-schema.org/draft-07/schema#",
    "type": "object",
    "properties": {
        "source_url": {
            "type": "string",
            "description": "URL of the webpage where the code fragment was found",
        },
        "code": {"type": "string", "description": "The code fragment content"},
        "language": {
            "type": "string",
            "description": "Programming language (e.g., 'csharp', 'python', 'cpp'). Auto-detected if not provided.",
        },
        "function_name": {
            "type": "string",
            "description": "Name of the function/method in the code fragment (used in filename)",
        },
        "context_tags": {
            "type": "array",
            "items": {"type": "string"},
            "description": "Contextual tags (e.g., ['dependency-injection', 'logging', 'async'])",
        },
        "additional_metadata": {
            "type": "object",
            "description": "Additional metadata to store with the fragment (e.g., {'framework': 'ASP.NET', 'version': '8.0'})",
        },
        "extraction_date": {
            "type": "string",
            "description": "ISO 8601 date when fragment was extracted. Auto-set if not provided.",
        },
    },
    "required": ["source_url", "code"],
}
```

### 2. Add MCP Tools to `main_mcp_server.py`

#### Add import at the top:
```python
from .repository_manager import RepositoryManager
```

#### Initialize repository manager in `main()` function (around line 282):
```python
# Initialize repository manager
repos_dir = os.getenv("REPOS_DIR", "/repos")
fragments_dir = os.getenv("CODE_FRAGMENTS_DIR", "/code_fragments")
repo_manager = RepositoryManager(repos_dir=repos_dir, fragments_dir=fragments_dir)
logger.info(f"Repository manager initialized: repos={repos_dir}, fragments={fragments_dir}")
```

#### Add new tools in `get_mcp_tools()` function (after line 208):
```python
        types.Tool(
            name="clone-and-index-repo",
            description="Clone a git repository and index it for code search. Supports public and private repositories (via SSH key).",
            inputSchema=mcp_json_schemas.CLONE_AND_INDEX_REPO_INPUT_SCHEMA,
        ),
        types.Tool(
            name="ingest-code-fragment",
            description="Store and index a code fragment from a webpage. Automatically detects language and creates metadata.",
            inputSchema=mcp_json_schemas.INGEST_CODE_FRAGMENT_INPUT_SCHEMA,
        ),
```

#### Add tool handlers in `call_tool()` function (after line 467):
```python
            elif name == "clone-and-index-repo":
                result = await clone_and_index_repo_tool(arguments, repo_manager)
            elif name == "ingest-code-fragment":
                result = await ingest_code_fragment_tool(arguments, repo_manager)
```

#### Add tool implementation functions (before `main()` function):
```python
async def clone_and_index_repo_tool(arguments: dict, repo_manager: RepositoryManager) -> dict:
    """
    MCP tool handler for cloning and indexing git repositories.
    """
    git_url = arguments.get("git_url")
    branch = arguments.get("branch")
    update_existing = arguments.get("update_existing", True)
    subdirectory = arguments.get("subdirectory")

    logger.info(f"Cloning repository: {git_url}")

    # Clone the repository
    result = repo_manager.clone_repository(
        git_url=git_url,
        branch=branch,
        update_existing=update_existing,
        subdirectory=subdirectory,
    )

    if result["status"] in ["success", "updated"]:
        # Trigger re-indexing of the repository
        # TODO: Integrate with cocoindex flow to re-scan the repo_path
        logger.info(f"Repository cloned/updated: {result['repo_path']}")

    return result


async def ingest_code_fragment_tool(arguments: dict, repo_manager: RepositoryManager) -> dict:
    """
    MCP tool handler for ingesting code fragments from webpages.
    """
    source_url = arguments.get("source_url")
    code = arguments.get("code")
    language = arguments.get("language")
    function_name = arguments.get("function_name")
    context_tags = arguments.get("context_tags", [])
    additional_metadata = arguments.get("additional_metadata", {})
    extraction_date = arguments.get("extraction_date")

    logger.info(f"Ingesting code fragment from: {source_url}")

    # Store the code fragment
    result = repo_manager.ingest_code_fragment(
        source_url=source_url,
        code=code,
        language=language,
        function_name=function_name,
        context_tags=context_tags,
        additional_metadata=additional_metadata,
        extraction_date=extraction_date,
    )

    if result["status"] == "success":
        # Trigger indexing of the code fragment
        # TODO: Integrate with cocoindex flow to index the file_path
        logger.info(f"Code fragment saved: {result['file_path']}")

    return result
```

### 3. Update `Dockerfile`

Replace the existing Dockerfile with:

```dockerfile
FROM python:3.12-slim

# System dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    gcc \
    git \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create app user
RUN useradd -m app

# Create directories for repos and code fragments
RUN mkdir -p /repos /code_fragments && chown -R app:app /repos /code_fragments

WORKDIR /app
COPY --chown=app:app . /app

# Install Python package from PyPI or build from source
USER app
RUN pip install --user --no-cache-dir cocoindex-code-mcp-server || \
    (pip install --user uv && uv sync --all-extras && uv run maturin develop)

# Expose MCP server port
EXPOSE 3033

# Run the MCP server
CMD ["python", "-m", "cocoindex_code_mcp_server.main_mcp_server", \
     "--port", "3033", \
     "/repos"]
```

### 4. Update `docker-compose.yml`

Replace with:

```yaml
version: "3.9"

services:
  postgres:
    image: pgvector/pgvector:pg16
    container_name: cocoindex-postgres
    restart: unless-stopped
    environment:
      POSTGRES_USER: cocoindex
      POSTGRES_PASSWORD: cocoindex
      POSTGRES_DB: cocoindex
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"

  cocoindex-mcp:
    build: .
    container_name: cocoindex-mcp-server
    restart: unless-stopped
    depends_on:
      - postgres
    environment:
      - COCOINDEX_DATABASE_URL=postgresql://cocoindex:cocoindex@postgres:5432/cocoindex
      - REPOS_DIR=/repos
      - CODE_FRAGMENTS_DIR=/code_fragments
      - GIT_SSH_KEY=/ssh/id_rsa  # Path to mounted SSH key
    ports:
      - "3033:3033"
    volumes:
      - repos:/repos  # Persistent storage for cloned repositories
      - code_fragments:/code_fragments  # Persistent storage for code fragments
      - ~/.ssh/id_rsa:/ssh/id_rsa:ro  # Mount SSH key for private repos (optional)

volumes:
  postgres_data:
  repos:
  code_fragments:
```

### 5. Integration with CocoIndex Flow

The repository manager creates/updates files in `/repos` and `/code_fragments`. To automatically index these:

**Option A: Add paths to main_mcp_server.py startup**

Modify the paths parameter to include both directories:
```python
paths = ["/repos", "/code_fragments"]
```

**Option B: Trigger re-indexing after clone/ingest**

In the tool handler functions, call the CocoIndex flow update mechanism after adding new files.

### 6. Testing

**Test git clone (public repo):**
```bash
# Via MCP tool call:
{
  "tool": "clone-and-index-repo",
  "arguments": {
    "git_url": "https://github.com/microsoft/semantic-kernel.git",
    "update_existing": true
  }
}
```

**Test code fragment ingestion:**
```bash
# Via MCP tool call:
{
  "tool": "ingest-code-fragment",
  "arguments": {
    "source_url": "https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/dependency-injection",
    "code": "public class MyService { ... }",
    "language": "csharp",
    "context_tags": ["dependency-injection", "dotnet"],
    "function_name": "MyService"
  }
}
```

## Docker Deployment

Build and run:
```bash
cd cocoindex-code-mcp-server
docker-compose up --build
```

The volumes will persist even when containers are stopped/rebuilt:
- `repos` → `/repos` in container
- `code_fragments` → `/code_fragments` in container
- `postgres_data` → PostgreSQL data

For pCloud backup, you can copy the Docker volume data to pCloud directory.

## Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `COCOINDEX_DATABASE_URL` | PostgreSQL connection string | Required |
| `REPOS_DIR` | Directory for cloned repositories | `/repos` |
| `CODE_FRAGMENTS_DIR` | Directory for code fragments | `/code_fragments` |
| `GIT_SSH_KEY` | Path to SSH private key for private repos | `/ssh/id_rsa` |

## Next Steps

1. Manually add the code changes outlined above
2. Build and test the Docker image
3. Test with Archon/Crawl4AI integration
4. Update CLAUDE.md with new tool documentation
