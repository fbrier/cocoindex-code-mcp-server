# Archon RAG Integration with CocoIndex MCP Server

## Overview

This guide explains how to integrate the **Archon RAG (Retrieval Augmented Generation) MCP Server** with the **CocoIndex Code MCP Server** to enable automated code discovery, indexing, and semantic search during knowledge retrieval operations.

### Integration Architecture

```
┌────────────────┐         ┌─────────────────┐         ┌──────────────────┐
│ Claude Session │────────>│   Archon RAG    │────────>│ CocoIndex MCP    │
│   (User)       │  Query  │   MCP Server    │  Ingest │     Server       │
└────────────────┘         └─────────────────┘         └──────────────────┘
                                   │                            │
                                   │                            │
                                   ▼                            ▼
                          ┌──────────────────┐        ┌──────────────────┐
                          │   Crawl4AI       │        │ PostgreSQL       │
                          │   Web Crawler    │        │ + pgvector       │
                          └──────────────────┘        │ (Indexed Code)   │
                                                      └──────────────────┘
```

**Flow:**
1. Claude session asks Archon RAG to search documentation
2. Archon RAG uses Crawl4AI to fetch web content
3. Crawl4AI detects code snippets and repository links
4. Code automatically ingested into CocoIndex
5. CocoIndex indexes code with semantic embeddings
6. Future queries can search indexed code semantically

## Use Cases

### 1. Documentation Crawling with Code Extraction

**Scenario:** User asks Claude to "learn about ASP.NET Core dependency injection"

**Flow:**
1. Archon RAG searches/crawls Microsoft Learn documentation
2. Crawl4AI detects code examples on the page
3. Code fragments automatically ingested into CocoIndex
4. User can later semantically search: "Find dependency injection examples in C#"

### 2. GitHub Repository Discovery

**Scenario:** User explores a framework's documentation

**Flow:**
1. Archon RAG crawls framework documentation
2. Detects GitHub repository links
3. Automatically clones and indexes repository
4. Enables semantic search across entire codebase

### 3. Tutorial/Blog Learning Path

**Scenario:** User follows a multi-part tutorial series

**Flow:**
1. Archon RAG crawls each tutorial page
2. Extracts all code examples with context
3. Builds searchable knowledge base of tutorial code
4. User can find "async database query examples from tutorial"

## MCP Server Integration Pattern

###For a New Claude Session Starting Fresh

When a new Claude Code session begins and needs to understand this integration:

**Step 1: Understand the Two MCP Servers**

```
┌─────────────────────────────────────────────────────────┐
│ Archon RAG MCP Server                                   │
│ - Provides RAG (knowledge retrieval) capabilities       │
│ - Uses Crawl4AI for web crawling                        │
│ - Stores documents in its own database                  │
│ - MCP tools: rag_search_knowledge_base, etc.            │
└─────────────────────────────────────────────────────────┘
                         │
                         │ Calls during crawling
                         ▼
┌─────────────────────────────────────────────────────────┐
│ CocoIndex Code MCP Server                               │
│ - Specializes in CODE indexing and semantic search     │
│ - Uses pgvector for vector similarity                   │
│ - Stores code with metadata in PostgreSQL              │
│ - MCP tools: ingest-code-fragment, clone-and-index-repo│
└─────────────────────────────────────────────────────────┘
```

**Step 2: Integration Point - Crawl4AI Hooks**

Archon RAG's Crawl4AI can be configured with hooks that run on each crawled page:

```python
# In Archon RAG server code (where Crawl4AI is configured)

from crawl4ai import AsyncWebCrawler

async def process_page_hook(url, html_content, metadata):
    """
    Hook called by Crawl4AI after fetching each page.

    This is where we integrate CocoIndex code extraction.
    """
    # Detect code fragments
    code_detector = CodeFragmentDetector()
    fragments = code_detector.detect_code_blocks(html_content, url)

    # Ingest into CocoIndex via MCP call
    async with MCPTestClient(host="192.168.1.11", port=3033) as cocoindex_client:
        for fragment in fragments:
            await cocoindex_client.call_tool("ingest-code-fragment", {
                "source_url": url,
                "code": fragment['code'],
                "language": fragment.get('language'),
                "context_tags": fragment.get('context_tags', []),
                "additional_metadata": {
                    "crawled_by": "archon_rag",
                    "page_title": metadata.get('title'),
                    "crawl_timestamp": metadata.get('timestamp')
                }
            })

    # Detect and clone repositories
    repo_detector = RepositoryDetector()
    repos = repo_detector.detect_repositories(html_content)

    for repo_url in repos:
        await cocoindex_client.call_tool("clone-and-index-repo", {
            "git_url": repo_url,
            "update_existing": True
        })

# Configure Crawl4AI with the hook
crawler = AsyncWebCrawler(hooks={
    'after_page_fetch': process_page_hook
})
```

**Step 3: Archon RAG Tool Extensions**

Extend Archon RAG MCP tools to optionally trigger code indexing:

```python
# In Archon RAG's MCP tool handlers

async def rag_search_knowledge_base_handler(arguments: dict):
    """
    Enhanced RAG search that auto-indexes discovered code.

    Arguments:
        query: Search query
        source_id: Optional source filter
        auto_index_code: If True, ingest code fragments (default: True)
    """
    query = arguments["query"]
    auto_index_code = arguments.get("auto_index_code", True)

    # Perform RAG search (existing logic)
    results = await archon_rag_search(query)

    # If auto_index_code enabled, process results for code
    if auto_index_code:
        for result in results:
            page_url = result.get('url')
            page_content = result.get('content')

            if page_content:
                # Extract and index code asynchronously
                asyncio.create_task(
                    extract_and_index_code(page_url, page_content)
                )

    return results
```

## Configuration

### Archon RAG Server Configuration

Add CocoIndex integration to Archon RAG's configuration:

```yaml
# archon_config.yaml

mcp_servers:
  archon_rag:
    # ... existing Archon RAG config

  cocoindex:
    enabled: true
    host: "192.168.1.11"
    port: 3033
    transport: "http"

code_extraction:
  enabled: true                    # Enable automatic code extraction
  min_code_length: 20              # Minimum characters
  max_code_length: 5000            # Maximum characters
  auto_index_repositories: true    # Auto-clone detected repos
  languages:                       # Languages to extract
    - "csharp"
    - "python"
    - "javascript"
    - "typescript"
    - "java"
    - "rust"
    - "go"

crawl4ai:
  hooks:
    enabled: true
    after_page_fetch: "cocoindex_integration.process_page_hook"
```

### Environment Variables

```bash
# .env file for Archon RAG

# CocoIndex MCP Server connection
COCOINDEX_MCP_HOST=192.168.1.11
COCOINDEX_MCP_PORT=3033
COCOINDEX_MCP_TRANSPORT=http

# Code extraction settings
COCOINDEX_AUTO_EXTRACT=true
COCOINDEX_MIN_CODE_LENGTH=20
COCOINDEX_AUTO_CLONE_REPOS=true

# Deduplication
COCOINDEX_DEDUPLICATE=true
COCOINDEX_CACHE_DURATION=3600  # seconds
```

## Implementation Example

### Complete Integration Module

Create `cocoindex_integration.py` in Archon RAG server:

```python
#!/usr/bin/env python3
"""
CocoIndex integration module for Archon RAG.

Automatically detects and ingests code fragments discovered
during RAG knowledge base building.
"""

import asyncio
import hashlib
import logging
from typing import Dict, List, Optional

from bs4 import BeautifulSoup
import re

logger = logging.getLogger(__name__)


class CocoIndexIntegration:
    """Handles code extraction and ingestion into CocoIndex."""

    def __init__(self, mcp_host: str, mcp_port: int, config: Optional[Dict] = None):
        self.mcp_host = mcp_host
        self.mcp_port = mcp_port
        self.config = config or {}
        self.seen_hashes = set()  # Deduplication

    async def process_crawled_page(self, url: str, html_content: str, metadata: Dict):
        """
        Process a crawled page for code extraction.

        Called by Crawl4AI hook.
        """
        if not self.config.get('enabled', True):
            return

        logger.info(f"Processing page for code extraction: {url}")

        stats = {
            'fragments_found': 0,
            'fragments_ingested': 0,
            'repos_found': 0,
            'repos_cloned': 0,
            'errors': []
        }

        try:
            # Import MCP client
            from tests.mcp_client import MCPTestClient

            async with MCPTestClient(
                host=self.mcp_host,
                port=self.mcp_port,
                transport='http'
            ) as mcp_client:

                # Extract code fragments
                fragments = self._detect_code_blocks(url, html_content, metadata)
                stats['fragments_found'] = len(fragments)

                for fragment in fragments:
                    if await self._ingest_fragment(mcp_client, fragment):
                        stats['fragments_ingested'] += 1

                # Extract repositories
                if self.config.get('auto_index_repositories', True):
                    repos = self._detect_repositories(html_content)
                    stats['repos_found'] = len(repos)

                    for repo_url in repos:
                        if await self._clone_repository(mcp_client, repo_url):
                            stats['repos_cloned'] += 1

        except Exception as e:
            logger.error(f"Error processing page {url}: {e}")
            stats['errors'].append(str(e))

        logger.info(f"Processed {url}: {stats['fragments_ingested']}/{stats['fragments_found']} fragments, "
                   f"{stats['repos_cloned']}/{stats['repos_found']} repos")

        return stats

    def _detect_code_blocks(self, url: str, html_content: str, metadata: Dict) -> List[Dict]:
        """Detect code blocks in HTML content."""
        soup = BeautifulSoup(html_content, 'html.parser')
        fragments = []

        for pre in soup.find_all('pre'):
            code_tag = pre.find('code')
            if not code_tag:
                continue

            code_text = code_tag.get_text()

            # Apply length filters
            min_len = self.config.get('min_code_length', 20)
            max_len = self.config.get('max_code_length', 5000)

            if not code_text or len(code_text) < min_len or len(code_text) > max_len:
                continue

            # Check for duplicate
            if self._is_duplicate(code_text):
                continue

            # Extract metadata
            language = self._detect_language(code_tag)
            context = self._extract_context(pre, soup)

            fragments.append({
                'source_url': url,
                'code': code_text,
                'language': language,
                'context_tags': context.get('tags', []),
                'function_name': context.get('function_name'),
                'additional_metadata': {
                    'heading': context.get('heading'),
                    'page_title': metadata.get('title'),
                    'crawled_by': 'archon_rag',
                    'extraction_timestamp': metadata.get('timestamp')
                }
            })

        return fragments

    def _detect_language(self, code_tag) -> Optional[str]:
        """Detect programming language from code tag classes."""
        class_attr = code_tag.get('class', [])
        if isinstance(class_attr, list):
            class_str = ' '.join(class_attr)
        else:
            class_str = class_attr

        # Language mapping
        patterns = {
            r'lang(?:uage)?-(?:cs|csharp|c#)': 'csharp',
            r'lang(?:uage)?-python': 'python',
            r'lang(?:uage)?-(?:cpp|c\+\+)': 'cpp',
            r'lang(?:uage)?-(?:js|javascript)': 'javascript',
            r'lang(?:uage)?-(?:ts|typescript)': 'typescript',
            r'lang(?:uage)?-java(?!script)': 'java',
            r'lang(?:uage)?-rust': 'rust',
            r'lang(?:uage)?-go': 'go',
        }

        for pattern, lang in patterns.items():
            if re.search(pattern, class_str, re.IGNORECASE):
                return lang

        return None  # Let CocoIndex auto-detect

    def _extract_context(self, element, soup) -> Dict:
        """Extract contextual information around code block."""
        context = {
            'tags': [],
            'heading': None,
            'function_name': None
        }

        # Find preceding heading
        heading = element.find_previous(['h1', 'h2', 'h3', 'h4'])
        if heading:
            context['heading'] = heading.get_text().strip()
            # Extract keywords as tags
            words = re.findall(r'\b[a-z]{4,}\b', context['heading'].lower())
            context['tags'] = words[:5]

        # Try to extract function name from code
        code_text = element.get_text()
        func_match = re.search(r'(?:def|function|public|private)\s+(\w+)\s*\(', code_text)
        if func_match:
            context['function_name'] = func_match.group(1)

        return context

    def _detect_repositories(self, html_content: str) -> List[str]:
        """Detect repository URLs in HTML."""
        patterns = [
            r'https://github\.com/[\w-]+/[\w.-]+',
            r'https://gitlab\.com/[\w-]+/[\w.-]+',
        ]

        repos = set()
        for pattern in patterns:
            repos.update(re.findall(pattern, html_content))

        # Clean URLs
        return [re.sub(r'\.git$', '', repo) for repo in repos]

    def _is_duplicate(self, code_text: str) -> bool:
        """Check if code has already been seen."""
        if not self.config.get('deduplicate', True):
            return False

        code_hash = hashlib.sha256(code_text.encode()).hexdigest()

        if code_hash in self.seen_hashes:
            return True

        self.seen_hashes.add(code_hash)
        return False

    async def _ingest_fragment(self, mcp_client, fragment: Dict) -> bool:
        """Ingest code fragment into CocoIndex."""
        try:
            result = await mcp_client.call_tool("ingest-code-fragment", fragment)

            if result.get('isError'):
                logger.error(f"Failed to ingest fragment: {result}")
                return False

            return True

        except Exception as e:
            logger.error(f"Exception ingesting fragment: {e}")
            return False

    async def _clone_repository(self, mcp_client, repo_url: str) -> bool:
        """Clone repository into CocoIndex."""
        try:
            result = await mcp_client.call_tool("clone-and-index-repo", {
                "git_url": repo_url,
                "update_existing": True
            })

            if result.get('isError'):
                logger.error(f"Failed to clone {repo_url}: {result}")
                return False

            return True

        except Exception as e:
            logger.error(f"Exception cloning {repo_url}: {e}")
            return False


# Hook function for Crawl4AI
_integration_instance = None

def initialize_integration(mcp_host: str, mcp_port: int, config: Dict):
    """Initialize the integration instance."""
    global _integration_instance
    _integration_instance = CocoIndexIntegration(mcp_host, mcp_port, config)


async def process_page_hook(url: str, html_content: str, metadata: Dict):
    """Hook called by Crawl4AI after fetching each page."""
    if _integration_instance:
        return await _integration_instance.process_crawled_page(url, html_content, metadata)
    else:
        logger.warning("CocoIndex integration not initialized")
        return {}
```

## User Guide for Claude Sessions

### For Claude Code Sessions Using This Integration

When you (Claude) encounter a new session that needs to use this integration:

**1. Understand the Setup:**
- Archon RAG MCP server is already configured with code extraction
- CocoIndex MCP server is running at 192.168.1.11:3033
- Crawl4AI hooks automatically extract code during web crawling

**2. When User Asks to Research Documentation:**

```
User: "Learn about ASP.NET Core dependency injection from Microsoft docs"

Claude Action:
1. Call Archon RAG tool: rag_search_knowledge_base("ASP.NET Core dependency injection")
2. Archon RAG crawls Microsoft Learn
3. Crawl4AI hook automatically extracts code examples
4. Code ingested into CocoIndex (happens automatically)
5. Return documentation results to user

Later, user can search code:
User: "Find C# dependency injection examples"
Claude: Calls CocoIndex search-vector tool with language="csharp"
```

**3. When User Asks About Repository:**

```
User: "Index the Semantic Kernel repository"

Claude Action:
Call CocoIndex tool: clone-and-index-repo({
  "git_url": "https://github.com/microsoft/semantic-kernel"
})

Result: Entire repository cloned and indexed for semantic search
```

**4. When User Wants to Search Indexed Code:**

```
User: "Find async database query examples"

Claude Action:
Call CocoIndex tool: search-vector({
  "query": "async database query transaction",
  "language": "csharp",  # or "python" etc.
  "top_k": 5
})

Result: Semantic search across all indexed code fragments and repositories
```

## Benefits of Integration

✅ **Automatic Knowledge Building**
- RAG search automatically builds code knowledge base
- No manual code ingestion needed
- Grows organically as user explores documentation

✅ **Semantic Code Search**
- Search code by meaning, not just keywords
- Find similar patterns across different codebases
- Language-aware embeddings

✅ **Context Preservation**
- Code fragments stored with source URL
- Heading/section context preserved
- Tags extracted from surrounding text

✅ **Repository Integration**
- Automatically clone discovered repositories
- Index entire codebases
- Keep repositories updated with git pull

## Testing

Test the integration end-to-end:

```python
async def test_archon_cocoindex_integration():
    """Test Archon RAG → CocoIndex integration."""

    # Initialize integration
    initialize_integration(
        mcp_host="192.168.1.11",
        mcp_port=3033,
        config={
            'enabled': True,
            'min_code_length': 20,
            'auto_index_repositories': True,
            'deduplicate': True
        }
    )

    # Simulate Crawl4AI crawling a page
    test_url = "https://learn.microsoft.com/en-us/aspnet/core/fundamentals/dependency-injection"
    test_html = """
    <h2>Configuring Services</h2>
    <p>Configure services in Startup.cs:</p>
    <pre><code class="language-csharp">
public void ConfigureServices(IServiceCollection services)
{
    services.AddTransient<IMyService, MyService>();
}
    </code></pre>

    <p>Repository: <a href="https://github.com/dotnet/aspnetcore">ASP.NET Core</a></p>
    """

    # Process page (as Crawl4AI hook would)
    stats = await process_page_hook(test_url, test_html, {'title': 'DI Documentation'})

    print(f"Processed page:")
    print(f"  Fragments ingested: {stats['fragments_ingested']}")
    print(f"  Repositories cloned: {stats['repos_cloned']}")

    # Verify code is searchable
    from tests.mcp_client import MCPTestClient

    async with MCPTestClient(host="192.168.1.11", port=3033) as client:
        search_result = await client.call_tool("search-vector", {
            "query": "dependency injection services",
            "language": "csharp",
            "top_k": 1
        })

        print(f"\nSearch results: {search_result}")
```

## Summary

Archon RAG and CocoIndex work together to create a self-building code knowledge base:
- Archon RAG handles document/web page retrieval
- CocoIndex handles code-specific semantic indexing
- Integration happens automatically via Crawl4AI hooks
- Users get both documentation search AND code search
- Knowledge base grows organically as users research topics
