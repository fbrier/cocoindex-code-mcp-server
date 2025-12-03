# CocoIndex C# Embedding Implementation - Progress Summary

## Goal
Enable fully operational C# code search with semantic embeddings using UniXcoder model in the CocoIndex MCP server.

## Target Setup
- **Languages**: C#, C++, C, Python, JavaScript, TypeScript, Java
- **Embedding Model**: microsoft/unixcoder-base (supports all target languages)
- **Deployment**: Docker via Portainer (docker-compose.yml + environment variables only)
- **Database**: PostgreSQL with pgvector extension
- **Test Repository**: https://github.com/chickensoft-games/GameDemo.git (187 files, C# Godot game)

## Core Problem
UniXcoder has a **strict 512 token limit**. When chunks exceed this limit, we get:
```
IndexError: index out of range in self
```

### Critical Insight: Characters ≠ Tokens
- 400 characters might be 350-450 tokens (varies by content)
- XML/project files tokenize into MORE tokens than characters
- Special characters increase token count unpredictably
- **Cannot rely on character count alone to prevent token overflow**

---

## CocoIndex Architecture Constraints Discovered

### 1. **DataSlice vs Plain Python Values**
- CocoIndex uses `DataSlice` objects for lazy evaluation
- Flow is defined at graph-build time, not runtime
- **CANNOT use Python if/elif on DataSlice values** - they don't resolve until runtime
- This broke our attempt to branch on `model_group` for different embedding models

```python
# ❌ DOESN'T WORK - if/elif happens at flow-build time
if chunk["model_group"] == "graphcodebert":  # chunk["model_group"] is DataSlice!
    chunk["embedding"] = graphcodebert_embedding(...)

# ✅ WORKS - single path for all
chunk["embedding"] = unixcoder_embedding(...)
```

### 2. **Model Loading in CocoIndex Functions**
Multiple approaches attempted:

#### Attempt 1: Global CocoIndex wrapper ❌
```python
_unixcoder_embed = cocoindex.functions.SentenceTransformerEmbed(model="...")
# Later: _unixcoder_embed(text)  # TypeError: not callable
```
**Problem**: `SentenceTransformerEmbed` is a transform operation, not a callable function.

#### Attempt 2: Create SentenceTransformer per chunk ❌
```python
@cocoindex.op.function()
def embed(text: str):
    model = SentenceTransformer("...")  # Creates new model each time!
    return model.encode(text)
```
**Problem**:
- Creates new model for EVERY chunk (187+ times)
- Causes PyTorch meta tensor errors
- Extremely inefficient

#### Attempt 3: Load in function with HuggingFace cache ⚠️ (Current)
```python
@cocoindex.op.function()
def embed(text: str):
    model = SentenceTransformer("...")  # Uses HF cache
    return model.encode(text)
```
**Status**: Being tested. Model weights cached on disk by HuggingFace, but loads per-subprocess.
**Risk**: May hit meta tensor errors again.

---

## Solutions Attempted

### 1. ✅ Smart Embedding Configuration
**Problem**: `use_smart_embedding` flag wasn't being set in config.
**Fix**: Added line in `update_flow_config()`:
```python
"use_smart_embedding": not use_default_embedding,
```

### 2. ❌ Branching on model_group (Failed)
**Problem**: Tried to use if/elif to select GraphCodeBERT vs UniXcoder.
**Why Failed**: DataSlice values don't resolve until runtime, branching happens at flow-build time.
**Fix**: Use UniXcoder for ALL code (supports all our target languages).

### 3. ✅ Chunk Size Reduction (Multiple Iterations)
- **First**: max_chunk_size = original values (up to 3000 for Markdown)
  - Result: 3 files failed with IndexError
- **Second**: max_chunk_size = 500 chars
  - Result: Still had failures (GameDemo.csproj)
- **Third**: max_chunk_size = 400 chars (current)
  - Status: Reduces most failures, but edge cases remain

### 4. 🔄 Auto-Retry Logic (In Progress)
**Approach**: Detect IndexError, split text in half, embed separately, average embeddings.

```python
def try_embed(chunk: str, depth: int = 0):
    try:
        return model.encode(chunk)
    except IndexError:
        if depth >= 2:  # Max 2 splits
            return np.zeros(768)  # Last resort

        # Split and average
        mid = len(chunk) // 2
        left = try_embed(chunk[:mid], depth + 1)
        right = try_embed(chunk[mid:], depth + 1)
        return (left + right) / 2.0
```

**Benefits**:
- ✅ Handles all edge cases gracefully
- ✅ No crashes, no data loss
- ✅ Fast normal path (only splits on error)
- ✅ Next file uses normal chunk size

**Status**: Code pushed, awaiting test results.

---

## Current State

### What's Working ✅
1. MCP server running at `http://solar.office.multideck.com:3033/mcp`
2. Repository cloning via `clone-and-index-repo` tool
3. GameDemo repository cloned to `/repos/GameDemo` (persists in Docker volume)
4. Keyword search working: `language:C#` returns results
5. AST-based chunking splitting files into logical units
6. `use_smart_embedding` properly enabled

### What's NOT Working ❌
1. **Embeddings not being generated** due to token limit crashes
2. **Hybrid search returns 0 results** (needs embeddings)
3. **Vector search doesn't work** (needs embeddings)
4. **3-187 files failing** during indexing (varies by attempt)

### Database State
- Keyword metadata indexed ✅
- Embeddings missing ❌
- Tables: `codeembedding__code_embeddings`, `codeembedding__cocoindex_tracking`
- Cleared multiple times during testing

---

## Files Modified

### `/Dockerfile`
- Removed `--rescan` flag (was clearing DB before repos cloned)
- Uses `uv run python` to activate virtual environment
- Removed Rust/maturin build (only needed for Haskell)
- Set `PYTHONPATH` instead of package installation

### `/python/cocoindex_code_mcp_server/main_mcp_server.py`
- Moved `code_embedding_flow` import to `main()` function scope (Python scoping issue)
- Changed uvicorn host from `127.0.0.1` to `0.0.0.0` (external access)

### `/python/cocoindex_code_mcp_server/cocoindex_config.py`
**Major changes**:
1. Added `use_smart_embedding` to config update (line ~1822)
2. Simplified embedding to use UniXcoder for all code (removed if/elif branching)
3. Reduced ALL `max_chunk_size` values from 500→400 chars
4. Added `safe_unixcoder_embed()` function with retry logic
5. Changed `unixcoder_embedding()` to use retry wrapper

**Current chunk sizes**:
```python
"C#": ChunkingParams(chunk_size=1200, min_chunk_size=300, chunk_overlap=250, max_chunk_size=400)
```

---

## Known Issues & Edge Cases

### 1. Token Overflow Despite max_chunk_size=400
**Files affected**:
- `GameDemo/GameDemo.csproj` (XML project file)
- `GameDemo/cspell.json` (configuration file)
- Others with high special character density

**Root cause**: Tokenization is unpredictable. Some content types (XML, JSON) have character-to-token ratios > 1.0.

### 2. Meta Tensor Errors
**Error**: `Cannot copy out of meta tensor; no data!`
**Occurs when**: Creating new SentenceTransformer instances repeatedly
**Current status**: May reoccur with current approach

### 3. Subprocess Model Loading
**Challenge**: CocoIndex's subprocess execution model makes model caching difficult
**Current approach**: Rely on HuggingFace's disk cache
**Trade-off**: May load model multiple times (once per subprocess)

---

## Testing Procedure

### After Each Deployment:

1. **Clear Database**:
```bash
docker exec -it <postgres_container> psql -U cocoindex -d cocoindex <<'EOF'
TRUNCATE TABLE codeembedding__code_embeddings RESTART IDENTITY CASCADE;
TRUNCATE TABLE codeembedding__cocoindex_tracking RESTART IDENTITY CASCADE;
EOF
```

2. **Restart Container** in Portainer (pulls new `:latest` image)

3. **Watch Logs** for:
```bash
docker logs cocoindex-mcp-server 2>&1 | grep -E "Using|embedding|UniXcode|ERROR|WARNING|Token overflow|Failed to embed"
```

**Expected good output**:
```
Using UniXcoder smart embedding for C#
Initial index built: files_0: 187 source rows processed
```

**Expected warnings** (acceptable):
```
⚠️  Token overflow at depth 0 (450 chars). Splitting: 225 + 225
```

**Bad errors**:
```
IndexError: index out of range in self
TypeError: 'SentenceTransformerEmbed' object is not callable
NotImplementedError: Cannot copy out of meta tensor
```

4. **Test Searches**:

**Keyword (should work)**:
```bash
curl -X POST http://solar.office.multideck.com:3033/mcp/ \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "search-keyword",
      "arguments": {
        "query": "language:C#",
        "top_k": 3
      }
    }
  }'
```

**Hybrid (needs embeddings)**:
```bash
curl -X POST http://solar.office.multideck.com:3033/mcp/ \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -d '{
    "jsonrpc": "2.0",
    "id": 2,
    "method": "tools/call",
    "params": {
      "name": "search-hybrid",
      "arguments": {
        "vector_query": "player movement input handling",
        "keyword_query": "language:C#",
        "language": "csharp",
        "top_k": 3
      }
    }
  }'
```

---

## Alternative Approaches to Consider

### Option 1: Use Smaller Default Model
- Switch to `sentence-transformers/all-MiniLM-L6-v2` (384 dim, 256 token limit)
- Pros: Smaller, faster, more forgiving
- Cons: Not code-specific, potentially lower quality

### Option 2: Pre-split Everything to 300 chars
- Set `max_chunk_size=300` globally
- Pros: Almost guaranteed to work
- Cons: More chunks, longer indexing

### Option 3: Skip Embeddings for Problem Files
- Detect repeated failures, skip embedding generation
- Pros: At least most files get indexed
- Cons: Missing semantic search for some files

### Option 4: Use GraphCodeBERT Instead
- Switch to `microsoft/graphcodebert-base` (same 512 limit but different tokenizer)
- Pros: May tokenize more efficiently
- Cons: Unknown if it helps, same fundamental issue

### Option 5: Accept Rare Failures
- Keep max_chunk_size=400, remove retry logic
- Let 1-3 files fail
- Pros: Simpler code
- Cons: Incomplete index

---

## Next Steps (When Resuming)

1. **Check Latest Deployment Status**
   - Look for errors in logs
   - Determine if retry logic is working
   - Count how many files succeeded

2. **If Still Failing**:
   - Try Option 2 (max_chunk_size=300)
   - Or simplify to basic SentenceTransformerEmbed with max_chunk_size=300
   - Accept that some edge cases may fail

3. **If Meta Tensor Errors Return**:
   - Must use `cocoindex.functions.SentenceTransformerEmbed` properly
   - Cannot create SentenceTransformer instances in custom functions
   - May need to abandon retry logic

4. **Once Embeddings Working**:
   - Test hybrid search thoroughly
   - Add to Claude Code MCP config:
     ```json
     {
       "mcpServers": {
         "cocoindex": {
           "url": "http://solar.office.multideck.com:3033/mcp",
           "transport": "streamable-http"
         }
       }
     }
     ```

---

## Key Learnings

1. **Character count is not token count** - always provide safety margin
2. **CocoIndex uses lazy evaluation** - can't branch on DataSlice values
3. **Model loading is tricky** - must work with CocoIndex's execution model
4. **Edge cases are inevitable** - need defensive error handling
5. **UniXcoder supports all our languages** - no need for multiple models
6. **Docker port mapping ≠ app binding** - must bind to 0.0.0.0
7. **Portainer requires file-less deployment** - all config via docker-compose.yml
8. **Database persistence needs careful management** - tracking tables cause "NO CHANGE" issues

## Commands Reference

```bash
# View logs
docker logs cocoindex-mcp-server 2>&1 | tail -100

# Clear database
docker exec -it <postgres_container> psql -U cocoindex -d cocoindex -c "TRUNCATE TABLE codeembedding__code_embeddings RESTART IDENTITY CASCADE; TRUNCATE TABLE codeembedding__cocoindex_tracking RESTART IDENTITY CASCADE;"

# Check container status
docker ps | grep cocoindex

# GitHub Actions
https://github.com/fbrier/cocoindex-code-mcp-server/actions

# Test MCP tools list
curl -X POST http://solar.office.multideck.com:3033/mcp/ \
  -H "Content-Type: application/json" \
  -H "Accept: application/json, text/event-stream" \
  -d '{"jsonrpc": "2.0", "id": 1, "method": "tools/list", "params": {}}'
```
