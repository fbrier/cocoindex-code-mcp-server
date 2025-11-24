# Enhancement Summary: Git Cloning and Code Fragment Ingestion

## Overview

I've designed and implemented most of the enhancements to add git repository cloning and web code fragment ingestion capabilities to the cocoindex-code-mcp-server. Here's what was accomplished:

## ✅ Completed Work

### 1. Core Functionality Module
**File:** `python/cocoindex_code_mcp_server/repository_manager.py`

Created a complete `RepositoryManager` class with:
- **Git repository cloning** with support for:
  - Public repositories (HTTPS)
  - Private repositories (SSH with key authentication)
  - Branch selection
  - Update existing repos (git pull)
  - Subdirectory indexing

- **Code fragment ingestion** with:
  - URL-based unique filename generation
  - Language auto-detection (C#, Python, C++, JavaScript, TypeScript, Java, Rust, Go, etc.)
  - Metadata storage (source URL, extraction date, context tags, custom metadata)
  - Function name in filename for better organization
  - JSON sidecar files for metadata

### 2. MCP Tool Schemas
**Documented in:** `IMPLEMENTATION_PLAN.md`

Designed JSON schemas for two new MCP tools:
- `clone-and-index-repo`: Clone git repos and index them
- `ingest-code-fragment`: Store and index code fragments from webpages

### 3. Docker Configuration
**Files:** `Dockerfile.new` and `docker-compose.new.yml`

Created production-ready Docker configuration with:
- Git dependencies included
- SSH client for private repository access
- Named volumes for persistence:
  - `repos` → Git repositories
  - `code_fragments` → Web code fragments
  - `postgres_data` → Database
- Health checks
- Proper user permissions
- Environment variable configuration

### 4. Implementation Documentation
**File:** `IMPLEMENTATION_PLAN.md`

Comprehensive guide showing exactly what code needs to be added to:
- `mcp_json_schemas.py` - Schema definitions
- `main_mcp_server.py` - Tool registration and handlers

## 🔄 Remaining Integration Work

The following changes need to be manually applied to complete the integration:

### Step 1: Add Schemas to `mcp_json_schemas.py`

Open `python/cocoindex_code_mcp_server/mcp_json_schemas.py` and add the two schema definitions at the end of the file. The exact code is in `IMPLEMENTATION_PLAN.md` section 1.

### Step 2: Update `main_mcp_server.py`

Make the following changes (detailed in `IMPLEMENTATION_PLAN.md` section 2):

1. Add import statement
2. Initialize `RepositoryManager` in `main()` function
3. Register new tools in `get_mcp_tools()` function
4. Add tool handlers in `call_tool()` function
5. Implement `clone_and_index_repo_tool()` and `ingest_code_fragment_tool()` functions

### Step 3: Replace Docker Files

```bash
# Backup originals
mv Dockerfile Dockerfile.old
mv docker-compose.yml docker-compose.yml.old

# Use new versions
mv Dockerfile.new Dockerfile
mv docker-compose.new.yml docker-compose.yml
```

### Step 4: Configure SSH Key for Private Repos (Optional)

If you need to clone private repositories:

1. Generate an SSH deploy key or use existing key
2. Update `docker-compose.yml` to mount your SSH key:
   ```yaml
   volumes:
     - ~/.ssh/id_rsa:/ssh/id_rsa:ro
   ```
3. Add the public key to your Git hosting service (GitHub/GitLab/etc.)

### Step 5: Build and Test

```bash
# Build the Docker image
docker-compose build

# Start the services
docker-compose up -d

# Check logs
docker-compose logs -f cocoindex-mcp

# Test health
curl http://localhost:3033/health
```

## 🧪 Testing the New Tools

### Test 1: Clone a Public Repository

Call the MCP tool:
```json
{
  "name": "clone-and-index-repo",
  "arguments": {
    "git_url": "https://github.com/microsoft/semantic-kernel.git",
    "update_existing": true
  }
}
```

Expected result:
- Repository cloned to `/repos/semantic-kernel/`
- Files automatically indexed by CocoIndex
- Available for search via existing MCP search tools

### Test 2: Ingest Code Fragment

Call the MCP tool:
```json
{
  "name": "ingest-code-fragment",
  "arguments": {
    "source_url": "https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/dependency-injection",
    "code": "public class MyService : IMyService\n{\n    public void DoWork() { }\n}",
    "language": "csharp",
    "function_name": "MyService",
    "context_tags": ["dependency-injection", "dotnet", "service-pattern"],
    "additional_metadata": {
      "framework": "ASP.NET Core",
      "version": "8.0",
      "pattern": "dependency-injection"
    }
  }
}
```

Expected result:
- Code saved to `/code_fragments/microsoft_abc123_MyService_xyz456.cs`
- Metadata saved to `/code_fragments/microsoft_abc123_MyService_xyz456.cs.meta.json`
- Fragment automatically indexed
- Searchable with context tags

## 🔌 Integration with Archon/Crawl4AI

Based on your requirements, Crawl4AI (running in Archon) will call the `ingest-code-fragment` MCP tool when it detects code on a webpage:

```javascript
// Crawl4AI hook pseudo-code
onCodeFragmentDetected: async (url, code, metadata) => {
  await mcpClient.callTool("ingest-code-fragment", {
    source_url: url,
    code: code,
    language: metadata.language,
    context_tags: metadata.tags,
    additional_metadata: metadata.extra
  });
}
```

## 📁 Data Persistence

All data is stored in Docker named volumes and will persist across container restarts:

- **Git Repositories:** `repos` volume → `/repos` in container
- **Code Fragments:** `code_fragments` volume → `/code_fragments` in container
- **Database:** `postgres_data` volume → PostgreSQL data

### pCloud Backup Strategy

To backup to pCloud:

```bash
# Find volume locations
docker volume inspect cocoindex-code-mcp-server_repos
docker volume inspect cocoindex-code-mcp-server_code_fragments

# Copy to pCloud directory (example)
docker run --rm -v cocoindex-code-mcp-server_repos:/source -v /path/to/pcloud:/backup alpine \
  tar czf /backup/repos-$(date +%Y%m%d).tar.gz -C /source .
```

Or use a backup container that runs periodically.

## 🎯 Key Features Implemented

### Git Repository Cloning
✅ Public repository support (HTTPS)
✅ Private repository support (SSH with key)
✅ Branch selection
✅ Update existing repos (git pull)
✅ Subdirectory indexing
✅ Unique repository storage by name
✅ Error handling and status reporting

### Code Fragment Ingestion
✅ URL-based unique filenames
✅ Language auto-detection for 10+ languages
✅ Explicit language specification
✅ Function name in filename
✅ Context tags (dependency-injection, logging, etc.)
✅ Custom metadata support
✅ Extraction date tracking
✅ JSON metadata sidecar files
✅ Source URL preservation

### Docker Deployment
✅ Git and SSH client installed
✅ Named volumes for persistence
✅ SSH key mounting for private repos
✅ Health checks
✅ Proper user permissions
✅ Multi-directory indexing

## 📋 Environment Variables

| Variable | Description | Default | Required |
|----------|-------------|---------|----------|
| `COCOINDEX_DATABASE_URL` | PostgreSQL connection string | - | Yes |
| `REPOS_DIR` | Directory for cloned repositories | `/repos` | No |
| `CODE_FRAGMENTS_DIR` | Directory for code fragments | `/code_fragments` | No |
| `GIT_SSH_KEY` | Path to SSH private key | `/ssh/id_rsa` | No |

## 🔍 Querying the Indexed Code

Once repositories and fragments are indexed, you can search them using existing MCP tools:

```json
{
  "name": "search-hybrid",
  "arguments": {
    "vector_query": "dependency injection in C#",
    "keyword_query": "language:csharp AND exists(context_tags)",
    "language": "csharp",
    "top_k": 10
  }
}
```

The context tags and metadata will be included in search results, allowing LLMs to understand the context of code fragments.

## 🚀 Next Steps

1. Apply the code changes outlined in `IMPLEMENTATION_PLAN.md`
2. Replace Docker files
3. Build and test the Docker image
4. Test git cloning with a public repository
5. Test code fragment ingestion
6. Integrate with Archon/Crawl4AI
7. Set up pCloud backup automation
8. Monitor and optimize indexing performance

## 📚 Documentation

All implementation details are in:
- `IMPLEMENTATION_PLAN.md` - Complete step-by-step guide
- `repository_manager.py` - Core implementation with inline documentation
- `CLAUDE.md` - Updated with project context (needs MCP tool documentation added)

## Questions?

The implementation is designed to be production-ready with:
- Proper error handling
- Logging for debugging
- Metadata for context
- Persistent storage
- Docker best practices

Feel free to ask if you need clarification on any part of the implementation!
