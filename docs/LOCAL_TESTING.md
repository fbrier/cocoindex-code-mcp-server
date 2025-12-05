# Local Testing Setup (Windows)

## Quick Start

Run tests locally in ~0.4 seconds instead of waiting 30-40 minutes for Docker build/deploy cycle.

## One-Time Setup

### 1. Install Test Dependencies

```bash
cd C:\Users\fbrier\Projects\cocoindex-code-mcp-server

# Install asyncpg from pre-built wheel (avoids C++ compilation)
uv pip install --only-binary :all: asyncpg

# Install pytest and database dependencies
uv pip install pytest pytest-cov pytest-asyncio psycopg[binary] pgvector
```

**Note**: We use `--only-binary :all:` for asyncpg because compiling from source requires Visual Studio C++ Build Tools and takes several minutes. Pre-built wheels install instantly.

## Running Tests

### All Tests
```bash
uv run pytest tests/ -v
```

### Specific Test File
```bash
uv run pytest tests/test_embedding_database_insertion.py -v
```

### With Coverage
```bash
uv run pytest tests/ --cov=cocoindex_code_mcp_server --cov-report=html
```

### Watch Mode (auto-rerun on file changes)
```bash
uv run pytest tests/ --looponfail
```

## Test Database Connection

Tests connect to the development PostgreSQL server:
- **Host**: solar.office.multideck.com
- **Port**: 5532
- **Database**: cocoindex
- **User**: cocoindex
- **Password**: cocoindex

Override via environment variables:
```bash
set COCOINDEX_TEST_DB_HOST=localhost
set COCOINDEX_TEST_DB_PORT=5432
uv run pytest tests/test_embedding_database_insertion.py -v
```

## Key Test Files

### `tests/test_embedding_database_insertion.py`
Verifies database embedding insertion behavior:
- ✅ Python lists insert successfully into pgvector
- ✅ Numpy arrays fail without `register_vector()`
- ✅ Vector similarity search works end-to-end

**Run time**: ~0.38 seconds

## Troubleshooting

### asyncpg Build Failures

**Error**: `error: command 'cl.exe' failed with exit code 2`

**Solution**: Install asyncpg from pre-built wheel:
```bash
uv pip install --only-binary :all: asyncpg
```

### Database Connection Errors

**Error**: `connection refused` or `timeout`

**Solution**: Check VPN connection to solar.office.multideck.com or verify database is running:
```bash
psql -h solar.office.multideck.com -p 5532 -U cocoindex -d cocoindex
```

### Import Errors

**Error**: `ModuleNotFoundError: No module named 'pytest'`

**Solution**: Ensure test dependencies are installed:
```bash
uv pip list | grep pytest
# If empty, run: uv pip install pytest pytest-cov pytest-asyncio
```

## Benefits of Local Testing

| Metric | Docker Build/Deploy | Local Testing |
|--------|---------------------|---------------|
| **Time** | 30-40 minutes | ~0.4 seconds |
| **Iteration Speed** | 1-2 cycles/hour | 100+ cycles/hour |
| **Debugging** | Difficult (container logs) | Easy (IDE debugger) |
| **CI/CD** | Required for every change | Optional (test first) |

## Test Development Workflow

1. **Write test** in `tests/` directory
2. **Run locally** with `uv run pytest tests/test_file.py -v`
3. **Iterate quickly** until tests pass (~seconds, not minutes)
4. **Commit** when tests pass locally
5. **Push** and let GitHub Actions verify in Docker environment

This workflow reduces feedback loop from hours to seconds, enabling rapid iteration on complex issues like the embedding conversion bug.

## Example: Debugging Embedding Issue

**Before local testing** (30-40 min per iteration):
1. Write fix → Commit → Push → GitHub Actions build (25 min) → Deploy (5 min) → Check logs → Repeat

**With local testing** (0.4 sec per iteration):
1. Write fix → Run tests locally (0.4 sec) → Iterate until pass → Commit → Push

**Result**: Found and fixed embedding conversion issue in 5 iterations (2 minutes) instead of 5 iterations (3+ hours).
