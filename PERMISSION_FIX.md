# Permission Fix for Log File Issue

## Problem

The container was restarting with this error:
```
PermissionError: [Errno 13] Permission denied: '/app/cocoindex_code_mcp_server.log'
```

## Root Cause

The application (`__init__.py:34`) tries to create a log file in the `WORKSPACE` directory (defaulting to current directory `/app`), but the `app` user doesn't have write permissions there.

## Solution Applied

### 1. Updated Dockerfile

**Changes made:**
- Added `/logs` directory creation with proper permissions
- Set `WORKSPACE=/logs` environment variable to redirect log files there

**Line 17-18:**
```dockerfile
RUN mkdir -p /repos /code_fragments /ssh /logs && \
    chown -R app:app /repos /code_fragments /ssh /logs
```

**Line 50:**
```dockerfile
ENV REPOS_DIR=/repos \
    CODE_FRAGMENTS_DIR=/code_fragments \
    GIT_SSH_KEY=/ssh/id_rsa \
    WORKSPACE=/logs
```

### 2. Updated docker-compose.yml

**Changes made:**
- Added `logs` volume for persistent log storage
- Added `WORKSPACE=/logs` environment variable
- Mounted logs volume to `/logs` in container

**Line 38:**
```yaml
- WORKSPACE=/logs
```

**Line 54:**
```yaml
- logs:/logs                            # Application logs
```

**Line 77-78:**
```yaml
logs:
  driver: local
```

## How It Works Now

1. Application reads `WORKSPACE` environment variable = `/logs`
2. Creates log file at `/logs/cocoindex_code_mcp_server.log`
3. `/logs` directory owned by `app` user (has write permissions)
4. Logs persist in Docker named volume `logs`

## To Deploy the Fix

### Option 1: Rebuild and Redeploy Locally

```bash
cd C:\Users\fbrier\Projects\cocoindex-code-mcp-server

# Stop current containers
docker-compose down

# Rebuild with fixed Dockerfile
docker-compose build

# Start services
docker-compose up -d

# Check logs
docker-compose logs -f cocoindex-mcp
```

### Option 2: Push to GitHub and Pull New Image

```bash
# Commit the fixed files
git add Dockerfile docker-compose.yml
git commit -m "Fix: Add /logs directory with proper permissions for log files

- Created /logs directory with app user permissions
- Set WORKSPACE=/logs environment variable
- Added logs volume to docker-compose.yml for persistence"

# Push to GitHub
git push origin main

# Wait for GitHub Actions to build (5-10 minutes)

# On deployment server, pull new image
docker pull ghcr.io/YOUR_USERNAME/cocoindex-code-mcp-server:latest

# Restart with new image
docker-compose down
docker-compose up -d
```

## Verification

After deployment, the container should start successfully:

```bash
# Check container is running
docker-compose ps

# Check logs - should see successful startup
docker-compose logs cocoindex-mcp

# Should see log file created
docker-compose exec cocoindex-mcp ls -la /logs/
```

Expected output:
```
-rw-r--r-- 1 app app <size> <date> cocoindex_code_mcp_server.log
```

## Log File Access

To view logs from the persistent volume:

```bash
# View logs directly
docker-compose exec cocoindex-mcp cat /logs/cocoindex_code_mcp_server.log

# Or tail logs in real-time
docker-compose exec cocoindex-mcp tail -f /logs/cocoindex_code_mcp_server.log
```

## Backup Logs

The logs are now in a named volume and will persist across container restarts:

```bash
# View volume location
docker volume inspect cocoindex-code-mcp-server_logs

# Backup logs to local directory
docker run --rm -v cocoindex-code-mcp-server_logs:/source -v $(pwd):/backup alpine \
  tar czf /backup/logs-backup.tar.gz -C /source .
```

## Files Modified

1. `Dockerfile` - Added `/logs` directory and `WORKSPACE` env var
2. `docker-compose.yml` - Added `logs` volume

## What This Doesn't Break

- All existing functionality remains intact
- Environment variables still work the same
- Volume persistence continues to work
- Only change is where log files are written

---

## Quick Deploy Commands

```bash
# If you're rebuilding locally
docker-compose down && docker-compose build && docker-compose up -d

# If you're using GitHub Container Registry
git add Dockerfile docker-compose.yml
git commit -m "Fix log file permissions"
git push origin main
# Wait for build, then:
docker pull ghcr.io/YOUR_USERNAME/cocoindex-code-mcp-server:latest
docker-compose down && docker-compose up -d
```
