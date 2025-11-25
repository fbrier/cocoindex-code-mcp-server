# Docker :latest Tag Fix

## Problem

When trying to pull the Docker image, `:latest` tag was not available:

```bash
# This failed
docker pull ghcr.io/YOUR_USERNAME/cocoindex-code-mcp-server:latest

# Had to use specific tag
docker pull ghcr.io/YOUR_USERNAME/cocoindex-code-mcp-server:main-2a2ce74
```

## Root Cause

The GitHub Actions workflow was only creating the `:latest` tag when pushing to the **default branch** of the repository.

**Original configuration (line 51):**
```yaml
type=raw,value=latest,enable={{is_default_branch}}
```

**The issue:**
- Workflow triggers on both `main` AND `master` branches
- But `:latest` only created when pushing to the default branch
- If your repo's default is `master` but you pushed to `main`, no `:latest` tag is created
- Only branch-specific tags created: `main`, `main-2a2ce74`

## Solution Applied

**Updated configuration:**
```yaml
type=raw,value=latest,enable=${{ github.ref == 'refs/heads/main' || github.ref == 'refs/heads/master' }}
```

**Now `:latest` is created when pushing to EITHER:**
- `main` branch, OR
- `master` branch

## Tags Created After Fix

When you push to `main` or `master`, these tags will be created:

1. **`latest`** - Always points to the latest build from main/master
2. **`main`** or **`master`** - Branch name
3. **`main-2a2ce74`** or **`master-abc123`** - Branch + commit SHA
4. **`v1.2.3`** - If you push a version tag (semver)

## How to Deploy the Fix

### Step 1: Commit and Push the Updated Workflow

```bash
cd C:\Users\fbrier\Projects\cocoindex-code-mcp-server

git add .github/workflows/docker-build.yml
git commit -m "Fix: Create :latest tag for both main and master branches"
git push origin main
```

### Step 2: Wait for GitHub Actions

1. Go to GitHub repository → **Actions** tab
2. Wait for "Build and Push Docker Image" to complete (~5-10 minutes)
3. The build will now create the `:latest` tag

### Step 3: Verify Tags

1. Go to GitHub repository → **Packages** (right sidebar)
2. Click on your package
3. You should now see these tags:
   - ✅ `latest`
   - ✅ `main` (or `master`)
   - ✅ `main-xxxxxxx` (SHA-specific)

### Step 4: Pull with :latest

Now this will work:

```bash
docker pull ghcr.io/YOUR_USERNAME/cocoindex-code-mcp-server:latest
```

## Understanding the Tag Strategy

### Branch-based Tags

- **`main`** - Latest build from main branch
- **`master`** - Latest build from master branch
- **`latest`** - Latest build from either main or master (whichever was last)

### SHA-based Tags (for reproducibility)

- **`main-2a2ce74`** - Specific commit on main branch
- **`master-abc123`** - Specific commit on master branch

### Version Tags (when you push git tags)

```bash
# Push a version tag
git tag v1.0.0
git push origin v1.0.0

# This creates:
# - v1.0.0
# - v1.0
# - v1
```

## Recommended Usage

### For Development

```yaml
# docker-compose.yml
services:
  cocoindex-mcp:
    image: ghcr.io/YOUR_USERNAME/cocoindex-code-mcp-server:latest
```

**Pros:** Always gets the newest version
**Cons:** May break if there are issues in latest build

### For Production (Recommended)

```yaml
# docker-compose.yml
services:
  cocoindex-mcp:
    image: ghcr.io/YOUR_USERNAME/cocoindex-code-mcp-server:main-2a2ce74
```

**Pros:** Reproducible, stable, can rollback easily
**Cons:** Need to manually update tag for new versions

### For Staging

```yaml
# docker-compose.yml
services:
  cocoindex-mcp:
    image: ghcr.io/YOUR_USERNAME/cocoindex-code-mcp-server:main
```

**Pros:** Always gets latest from main branch
**Cons:** Updates automatically (may be unexpected)

## Checking Which Tags Exist

### Via GitHub Web Interface

1. Go to your repository
2. Click **Packages** (right sidebar)
3. Click on package name
4. See all available tags

### Via Docker CLI

```bash
# List all tags for your image
docker image ls ghcr.io/YOUR_USERNAME/cocoindex-code-mcp-server

# Or use GitHub API
curl https://api.github.com/users/YOUR_USERNAME/packages/container/cocoindex-code-mcp-server/versions
```

## Troubleshooting

### :latest still not available after workflow runs

**Check:**
1. Did the workflow complete successfully? (Green checkmark in Actions tab)
2. Was it a push to `main` or `master` branch?
3. Was it a pull request? (PRs don't push images)

**Solution:**
```bash
# Make a small change and push again
git commit --allow-empty -m "Trigger rebuild for :latest tag"
git push origin main
```

### Multiple :latest tags for different branches

**This is intentional.** When you push to `master`, it updates `:latest`. When you push to `main`, it updates `:latest`. The last push wins.

**If you want separate tags:**
- Use `main` tag for main branch builds
- Use `master` tag for master branch builds
- Use SHA tags for specific versions: `main-2a2ce74`

### Image pulled but wrong version

**Check what you have:**
```bash
docker image inspect ghcr.io/YOUR_USERNAME/cocoindex-code-mcp-server:latest
```

**Force pull latest:**
```bash
docker pull ghcr.io/YOUR_USERNAME/cocoindex-code-mcp-server:latest
docker-compose down
docker-compose up -d
```

## Summary

**Before fix:**
- `:latest` only created for default branch
- Had to use SHA-specific tags like `main-2a2ce74`

**After fix:**
- `:latest` created for both `main` and `master` branches
- Can use simple `docker pull :latest`
- More convenient deployment

**Next step:**
```bash
git add .github/workflows/docker-build.yml
git commit -m "Fix: Create :latest tag for main and master branches"
git push origin main
```

Then wait for build and use `:latest` tag! 🎉
