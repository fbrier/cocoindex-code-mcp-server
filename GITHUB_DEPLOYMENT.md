# GitHub Container Registry Deployment Guide

## Overview

Since Docker can't run locally due to nested virtualization limitations, we're using **GitHub Actions** to build the Docker image and push it to **GitHub Container Registry (GHCR)**.

## What Was Set Up

### New GitHub Actions Workflow: `.github/workflows/docker-build.yml`

This workflow:
- ✅ Builds Docker image on GitHub's infrastructure
- ✅ Pushes to GitHub Container Registry (`ghcr.io`)
- ✅ Runs on push to `main`/`master` branch
- ✅ Creates multiple tags (latest, version, sha)
- ✅ Supports multi-architecture builds (amd64, arm64)
- ✅ Uses caching for faster builds

## Step-by-Step Deployment

### Step 1: Commit and Push Your Changes

From your Windows VM:

```bash
cd C:\Users\fbrier\Projects\cocoindex-code-mcp-server

# Add all changed files
git add .

# Commit with a descriptive message
git commit -m "Add git cloning and code fragment ingestion features

- Added repository_manager.py for git operations
- Added two new MCP tools: clone-and-index-repo and ingest-code-fragment
- Updated Dockerfile and docker-compose.yml for enhanced deployment
- Added GitHub Actions workflow for Docker image building"

# Push to GitHub
git push origin main
```

**Note:** Replace `main` with your branch name if different (might be `master`).

### Step 2: Monitor GitHub Actions Build

1. Go to your GitHub repository in a browser
2. Click on the **"Actions"** tab
3. You should see a new workflow run called "Build and Push Docker Image"
4. Click on it to watch the build progress

**Build takes approximately 5-10 minutes.**

### Step 3: Verify Image was Pushed

After the build completes:

1. Go to your repository on GitHub
2. Click on **"Packages"** (right sidebar)
3. You should see `cocoindex-code-mcp-server` package
4. Click on it to see available tags:
   - `latest` - Latest build from main branch
   - `main` - Latest build from main branch
   - `sha-xxxxxxx` - Specific commit

### Step 4: Deploy on Your Server (with Docker)

On a machine that HAS Docker running (e.g., your Gitea server or production server):

**Option A: Use docker-compose with GitHub image**

Create a `docker-compose.prod.yml`:

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
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U cocoindex"]
      interval: 10s
      timeout: 5s
      retries: 5

  cocoindex-mcp:
    image: ghcr.io/YOUR_GITHUB_USERNAME/cocoindex-code-mcp-server:latest
    container_name: cocoindex-mcp-server
    restart: unless-stopped
    depends_on:
      postgres:
        condition: service_healthy
    environment:
      - COCOINDEX_DATABASE_URL=postgresql://cocoindex:cocoindex@postgres:5432/cocoindex
      - REPOS_DIR=/repos
      - CODE_FRAGMENTS_DIR=/code_fragments
      - GIT_SSH_KEY=/ssh/id_rsa
    ports:
      - "3033:3033"
    volumes:
      - repos:/repos
      - code_fragments:/code_fragments
      # Mount SSH key if needed for private repos
      # - ~/.ssh/id_rsa:/ssh/id_rsa:ro

volumes:
  postgres_data:
  repos:
  code_fragments:
```

**Replace `YOUR_GITHUB_USERNAME`** with your actual GitHub username!

**Deploy:**

```bash
# Pull latest image
docker-compose -f docker-compose.prod.yml pull

# Start services
docker-compose -f docker-compose.prod.yml up -d

# Check logs
docker-compose -f docker-compose.prod.yml logs -f
```

**Option B: Use Docker run directly**

```bash
# Create network
docker network create cocoindex-network

# Start PostgreSQL
docker run -d \
  --name cocoindex-postgres \
  --network cocoindex-network \
  -e POSTGRES_USER=cocoindex \
  -e POSTGRES_PASSWORD=cocoindex \
  -e POSTGRES_DB=cocoindex \
  -v postgres_data:/var/lib/postgresql/data \
  -p 5432:5432 \
  pgvector/pgvector:pg16

# Start MCP server
docker run -d \
  --name cocoindex-mcp-server \
  --network cocoindex-network \
  -e COCOINDEX_DATABASE_URL=postgresql://cocoindex:cocoindex@cocoindex-postgres:5432/cocoindex \
  -e REPOS_DIR=/repos \
  -e CODE_FRAGMENTS_DIR=/code_fragments \
  -v repos:/repos \
  -v code_fragments:/code_fragments \
  -p 3033:3033 \
  ghcr.io/YOUR_GITHUB_USERNAME/cocoindex-code-mcp-server:latest
```

### Step 5: Test the Deployment

```bash
# Test health endpoint
curl http://localhost:3033/health

# Check if services are running
docker ps

# View logs
docker logs cocoindex-mcp-server
```

### Step 6: Configure Claude Code

Update your `.mcp.json`:

```json
{
  "mcpServers": {
    "cocoindex-rag": {
      "command": "pnpm",
      "args": [
        "dlx",
        "mcp-remote@next",
        "http://YOUR_SERVER_IP:3033/mcp"
      ]
    }
  }
}
```

Replace `YOUR_SERVER_IP` with:
- `localhost` if running locally
- Your server's IP address if running remotely

## Authentication for Private Repository

If your fork is private, you need to authenticate to pull from GHCR:

### On Your Deployment Server:

```bash
# Create a GitHub Personal Access Token (PAT) with 'read:packages' scope
# Then log in to GHCR
echo YOUR_GITHUB_PAT | docker login ghcr.io -u YOUR_GITHUB_USERNAME --password-stdin

# Now you can pull private images
docker pull ghcr.io/YOUR_GITHUB_USERNAME/cocoindex-code-mcp-server:latest
```

## Updating to Latest Version

When you push new changes to GitHub:

```bash
# On deployment server
docker-compose -f docker-compose.prod.yml pull
docker-compose -f docker-compose.prod.yml up -d

# Or with docker run
docker pull ghcr.io/YOUR_GITHUB_USERNAME/cocoindex-code-mcp-server:latest
docker stop cocoindex-mcp-server
docker rm cocoindex-mcp-server
# Run the docker run command again from Step 4
```

## Troubleshooting

### Issue: GitHub Actions build fails

**Check:**
- Go to Actions tab on GitHub
- Click on failed build
- Look at the error message

**Common fixes:**
- Ensure Dockerfile is valid
- Check if all files are committed and pushed

### Issue: Can't pull image - "access denied"

**Solution:**
1. Make sure the repository package is public, OR
2. Authenticate with GHCR (see "Authentication for Private Repository" section)

### Issue: Image pulled but container won't start

**Check logs:**
```bash
docker logs cocoindex-mcp-server
```

**Common issues:**
- Database connection: Check `COCOINDEX_DATABASE_URL`
- PostgreSQL not ready: Wait for it to fully start
- Ports already in use: Check with `netstat -an | grep 3033`

## Image Tags Explained

The GitHub Actions workflow creates several tags:

- `latest` - Always points to the latest build from main/master branch
- `main` or `master` - Latest build from that branch
- `v1.0.0` - Semantic version tags (if you push tags)
- `sha-abc123` - Specific commit SHA for reproducibility

**Recommended for production:** Use specific version tags or SHA tags for stability.

**Recommended for development:** Use `latest` tag for always getting the newest version.

## Architecture

```
Your Windows VM (Git only)
    |
    | git push
    v
GitHub Repository
    |
    | triggers
    v
GitHub Actions (Ubuntu runner with Docker)
    |
    | builds
    v
Docker Image
    |
    | pushes to
    v
GitHub Container Registry (ghcr.io)
    |
    | docker pull
    v
Your Deployment Server (Gitea server, etc.)
    |
    | docker run
    v
Running Containers
```

## Summary

✅ **What's Working:**
- GitHub Actions automatically builds Docker images
- Images pushed to GitHub Container Registry
- Can deploy on any server with Docker

❌ **What's Not Working:**
- Building Docker locally (nested virtualization limitation)

✅ **Solution:**
- Use GitHub as your build infrastructure
- Deploy pre-built images to servers that support Docker

## Next Steps

1. Commit and push your changes
2. Wait for GitHub Actions to build
3. Deploy on a server with Docker support
4. Test the new MCP tools!

See **INTEGRATION_COMPLETE.md** for testing the new features once deployed.
