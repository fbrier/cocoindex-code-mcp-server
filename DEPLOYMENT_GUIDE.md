# Deployment Guide - CocoIndex Code MCP Server

## ✅ Completed Integration Steps

All code changes have been successfully applied:

1. ✅ **Schemas Added** (`mcp_json_schemas.py`)
   - `CLONE_AND_INDEX_REPO_INPUT_SCHEMA`
   - `INGEST_CODE_FRAGMENT_INPUT_SCHEMA`

2. ✅ **Core Module Created** (`repository_manager.py`)
   - Git repository cloning with SSH key support
   - Code fragment storage with metadata
   - Language auto-detection

3. ✅ **Main Server Updated** (`main_mcp_server.py`)
   - RepositoryManager import added
   - Initialization code added
   - Two new MCP tools registered
   - Tool handlers added
   - Implementation functions added

4. ✅ **Docker Configuration Updated**
   - `Dockerfile` - Includes git, SSH client, named volumes
   - `docker-compose.yml` - PostgreSQL + MCP server with volumes

5. ✅ **Documentation Updated**
   - `CLAUDE.md` - New MCP tools documented
   - `IMPLEMENTATION_PLAN.md` - Complete implementation guide
   - `ENHANCEMENT_SUMMARY.md` - Feature overview

## 🚀 Next Steps - Manual Deployment

Since Docker is not available in this Git Bash environment, you'll need to complete the deployment manually. Here's how:

### Step 1: Verify Docker Desktop is Running

Open PowerShell or Command Prompt:

```powershell
docker --version
docker-compose --version
```

If not installed, download Docker Desktop from: https://www.docker.com/products/docker-desktop

### Step 2: Navigate to Project Directory

```powershell
cd C:\Users\fbrier\Projects\cocoindex-code-mcp-server
```

### Step 3: Build the Docker Image

```powershell
docker-compose build
```

This will:
- Build the Python 3.12 container
- Install git and SSH client
- Install cocoindex-code-mcp-server from PyPI (or build from source)
- Set up directories for repos and code fragments

**Expected output:**
- Build should complete in 5-10 minutes
- Final message: `Successfully tagged cocoindex-code-mcp-server_cocoindex-mcp:latest`

### Step 4: Start the Services

```powershell
docker-compose up -d
```

This will start:
- PostgreSQL database with pgvector (port 5432)
- CocoIndex MCP server (port 3033)

**Check logs:**
```powershell
docker-compose logs -f
```

Look for:
- `✅ Successfully retrieved resource` - MCP server is ready
- `Repository manager initialized` - Git cloning feature is active
- `Hybrid search engine initialized` - Search is ready

### Step 5: Test the MCP Server

**Option A: Test with curl (simple health check)**

```powershell
curl http://localhost:3033/health
```

**Option B: Test with MCP Client**

From your Claude Code `.mcp.json`:

```json
{
  "mcpServers": {
    "cocoindex-rag": {
      "command": "pnpm",
      "args": [
        "dlx",
        "mcp-remote@next",
        "http://localhost:3033/mcp"
      ]
    }
  }
}
```

Then restart Claude Code and test the new tools.

### Step 6: Test New MCP Tools

**Test 1: Clone a Public Repository**

```json
{
  "name": "clone-and-index-repo",
  "arguments": {
    "git_url": "https://github.com/microsoft/semantic-kernel.git",
    "update_existing": true
  }
}
```

**Test 2: Ingest a Code Fragment**

```json
{
  "name": "ingest-code-fragment",
  "arguments": {
    "source_url": "https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/",
    "code": "public async Task<string> GetDataAsync()\n{\n    using HttpClient client = new();\n    string result = await client.GetStringAsync(\"https://example.com\");\n    return result;\n}",
    "language": "csharp",
    "function_name": "GetDataAsync",
    "context_tags": ["async", "httpclient", "dotnet"],
    "additional_metadata": {
      "framework": "ASP.NET Core",
      "pattern": "async-await"
    }
  }
}
```

### Step 7: Verify Data Persistence

Check Docker volumes:

```powershell
docker volume ls
```

You should see:
- `cocoindex-code-mcp-server_postgres_data`
- `cocoindex-code-mcp-server_repos`
- `cocoindex-code-mcp-server_code_fragments`

Inspect a volume:

```powershell
docker volume inspect cocoindex-code-mcp-server_repos
```

## 🔧 Troubleshooting

### Issue: Build fails with "permission denied"

**Solution:** Ensure Docker Desktop is running and you have admin privileges.

### Issue: Container exits immediately

**Solution:** Check logs for database connection errors:

```powershell
docker-compose logs cocoindex-mcp
```

Ensure `COCOINDEX_DATABASE_URL` is set correctly in `.env` file.

### Issue: PostgreSQL connection refused

**Solution:** Wait for PostgreSQL to fully start:

```powershell
docker-compose logs postgres
```

Look for: `database system is ready to accept connections`

### Issue: Git clone fails with SSH key error

**Solution:** Mount your SSH key in `docker-compose.yml`:

```yaml
volumes:
  - ~/.ssh/id_rsa:/ssh/id_rsa:ro
```

On Windows, use full path:

```yaml
volumes:
  - C:/Users/fbrier/.ssh/id_rsa:/ssh/id_rsa:ro
```

## 📊 Monitoring

### View Running Containers

```powershell
docker-compose ps
```

### View Logs

```powershell
# All services
docker-compose logs -f

# Just MCP server
docker-compose logs -f cocoindex-mcp

# Just PostgreSQL
docker-compose logs -f postgres
```

### Execute Commands Inside Container

```powershell
# Open shell in MCP container
docker-compose exec cocoindex-mcp bash

# List cloned repositories
docker-compose exec cocoindex-mcp ls -la /repos

# List code fragments
docker-compose exec cocoindex-mcp ls -la /code_fragments
```

## 🔄 Updating and Rebuilding

When you make code changes:

```powershell
# Stop services
docker-compose down

# Rebuild image
docker-compose build

# Start services
docker-compose up -d
```

**Note:** Volumes persist, so your cloned repos and code fragments won't be lost.

## 🗑️ Cleanup

### Stop and remove containers (keeps volumes):

```powershell
docker-compose down
```

### Remove everything including volumes:

```powershell
docker-compose down -v
```

**Warning:** This deletes all cloned repositories and code fragments!

## 📦 Backup Strategy for pCloud

### Option 1: Export Volume Data

```powershell
# Create backup directory
mkdir C:\pCloud\cocoindex-backups

# Export repos volume
docker run --rm -v cocoindex-code-mcp-server_repos:/source -v C:\pCloud\cocoindex-backups:/backup alpine tar czf /backup/repos-backup.tar.gz -C /source .

# Export code_fragments volume
docker run --rm -v cocoindex-code-mcp-server_code_fragments:/source -v C:\pCloud\cocoindex-backups:/backup alpine tar czf /backup/fragments-backup.tar.gz -C /source .

# Export postgres data
docker run --rm -v cocoindex-code-mcp-server_postgres_data:/source -v C:\pCloud\cocoindex-backups:/backup alpine tar czf /backup/postgres-backup.tar.gz -C /source .
```

### Option 2: Direct Volume Backup (Recommended)

Use a backup container that runs periodically:

Create `backup-to-pcloud.ps1`:

```powershell
$date = Get-Date -Format "yyyy-MM-dd"
$backupDir = "C:\pCloud\cocoindex-backups\$date"
New-Item -ItemType Directory -Path $backupDir -Force

# Backup each volume
docker run --rm -v cocoindex-code-mcp-server_repos:/source -v ${backupDir}:/backup alpine tar czf /backup/repos.tar.gz -C /source .
docker run --rm -v cocoindex-code-mcp-server_code_fragments:/source -v ${backupDir}:/backup alpine tar czf /backup/fragments.tar.gz -C /source .
docker run --rm -v cocoindex-code-mcp-server_postgres_data:/source -v ${backupDir}:/backup alpine tar czf /backup/postgres.tar.gz -C /source .

Write-Host "Backup completed: $backupDir"
```

Schedule this script to run daily using Windows Task Scheduler.

## 🌐 Integration with Archon/Crawl4AI

When Crawl4AI detects code fragments, it should call the MCP server:

```javascript
// Crawl4AI hook configuration
{
  mcpServerUrl: "http://localhost:3033/mcp",
  onCodeFragmentDetected: async (fragment) => {
    await mcpClient.callTool("ingest-code-fragment", {
      source_url: fragment.url,
      code: fragment.code,
      language: fragment.detectedLanguage,
      context_tags: fragment.tags,
      additional_metadata: {
        crawled_at: new Date().toISOString(),
        page_title: fragment.pageTitle
      }
    });
  }
}
```

## 📝 Summary

**What's Working:**
- ✅ All code changes applied
- ✅ Docker configuration ready
- ✅ New MCP tools implemented
- ✅ Documentation updated

**What You Need to Do:**
1. Open PowerShell/Command Prompt
2. Run `docker-compose build`
3. Run `docker-compose up -d`
4. Test the new MCP tools
5. Integrate with Archon/Crawl4AI
6. Set up pCloud backup automation

**Files Modified:**
- `python/cocoindex_code_mcp_server/mcp_json_schemas.py`
- `python/cocoindex_code_mcp_server/main_mcp_server.py`
- `python/cocoindex_code_mcp_server/repository_manager.py` (new)
- `Dockerfile`
- `docker-compose.yml`
- `CLAUDE.md`

All changes are ready for deployment! 🚀
