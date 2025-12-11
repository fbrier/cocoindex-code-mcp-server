# Crawl4AI Integration with CocoIndex MCP Server

## Overview

This guide explains how to integrate **Crawl4AI** (web crawler/scraper) with the **CocoIndex MCP Server** to automatically detect, extract, and index code snippets and repositories discovered during web crawling.

### Use Case

When Crawl4AI crawls documentation sites, blogs, GitHub pages, or learning resources, it can:
1. **Detect code snippets** in web pages (syntax-highlighted blocks, `<pre>`, `<code>` tags)
2. **Extract code fragments** with metadata (URL, language, context)
3. **Auto-ingest** into CocoIndex for semantic search
4. **Detect repository links** (GitHub, GitLab, etc.)
5. **Auto-clone and index** repositories

## Architecture

```
┌─────────────┐      ┌──────────────┐      ┌─────────────────────┐
│  Crawl4AI   │─────>│ Code Detector│─────>│ CocoIndex MCP Server│
│   Crawler   │      │   & Parser   │      │                     │
└─────────────┘      └──────────────┘      └─────────────────────┘
       │                                              │
       │                                              ▼
       │                                    ┌──────────────────┐
       └───────────────────────────────────>│ PostgreSQL       │
                                            │ + pgvector       │
                                            │ (Indexed Code)   │
                                            └──────────────────┘
```

## CocoIndex MCP Tools for Crawl4AI

### 1. `ingest-code-fragment` - For Code Snippets

Stores and indexes code fragments discovered on web pages.

**When to Use:**
- Found code snippet in documentation page
- Code example in blog post or tutorial
- Stack Overflow answer with code
- API documentation with sample code

**Tool Definition:**
```json
{
  "name": "ingest-code-fragment",
  "description": "Store and index a code fragment from a webpage. Automatically detects language and creates metadata.",
  "inputSchema": {
    "type": "object",
    "properties": {
      "source_url": {
        "type": "string",
        "description": "URL of the webpage where the code fragment was found",
        "required": true
      },
      "code": {
        "type": "string",
        "description": "The code fragment content",
        "required": true
      },
      "language": {
        "type": "string",
        "description": "Programming language (e.g., 'csharp', 'python', 'cpp'). Auto-detected if not provided."
      },
      "function_name": {
        "type": "string",
        "description": "Name of the function/method in the code fragment (used in filename)"
      },
      "context_tags": {
        "type": "array",
        "items": {"type": "string"},
        "description": "Contextual tags (e.g., ['dependency-injection', 'logging', 'async'])"
      },
      "additional_metadata": {
        "type": "object",
        "description": "Additional metadata to store with the fragment (e.g., {'framework': 'ASP.NET', 'version': '8.0'})"
      },
      "extraction_date": {
        "type": "string",
        "description": "ISO 8601 date when fragment was extracted. Auto-set if not provided."
      }
    },
    "required": ["source_url", "code"]
  }
}
```

**Example Call:**
```python
import asyncio
from mcp_client import MCPTestClient

async def ingest_code_snippet():
    async with MCPTestClient(host="192.168.1.11", port=3033, transport='http') as client:
        result = await client.call_tool("ingest-code-fragment", {
            "source_url": "https://learn.microsoft.com/en-us/aspnet/core/fundamentals/dependency-injection",
            "code": """public class Startup
{
    public void ConfigureServices(IServiceCollection services)
    {
        services.AddTransient<ITransientService, TransientService>();
        services.AddScoped<IScopedService, ScopedService>();
        services.AddSingleton<ISingletonService, SingletonService>();
    }
}""",
            "language": "csharp",  # or "C#" or "cs" - all normalize correctly
            "function_name": "ConfigureServices",
            "context_tags": ["dependency-injection", "dotnet", "startup"],
            "additional_metadata": {
                "framework": "ASP.NET Core",
                "version": "8.0",
                "topic": "Dependency Injection"
            }
        })
        print(result)
```

### 2. `clone-and-index-repo` - For GitHub Repositories

Clones a git repository and automatically indexes all code files.

**When to Use:**
- Found GitHub repository link in documentation
- Tutorial references a sample project
- Open source library documentation
- Code examples repository

**Tool Definition:**
```json
{
  "name": "clone-and-index-repo",
  "description": "Clone a git repository and index it for code search. Supports public and private repositories (via SSH key).",
  "inputSchema": {
    "type": "object",
    "properties": {
      "git_url": {
        "type": "string",
        "description": "Git repository URL (https:// or git@). Supports both public and private repositories.",
        "required": true
      },
      "branch": {
        "type": "string",
        "description": "Branch to clone (default: main or master). If not specified, will use repository default branch."
      },
      "update_existing": {
        "type": "boolean",
        "description": "If true, performs git pull on existing repository instead of failing. Default: true",
        "default": true
      },
      "subdirectory": {
        "type": "string",
        "description": "Optional subdirectory within the repo to index (indexes entire repo if not specified)"
      }
    },
    "required": ["git_url"]
  }
}
```

**Example Call:**
```python
async def clone_repository():
    async with MCPTestClient(host="192.168.1.11", port=3033, transport='http') as client:
        result = await client.call_tool("clone-and-index-repo", {
            "git_url": "https://github.com/microsoft/semantic-kernel.git",
            "branch": "main",
            "update_existing": true,
            "subdirectory": "dotnet/src"  # Index only C# code
        })
        print(result)
```

## Crawl4AI Integration Patterns

### Pattern 1: Code Snippet Detection

Detect code snippets in HTML and ingest them.

```python
from bs4 import BeautifulSoup
import re

class CodeFragmentDetector:
    """Detects code fragments in HTML content."""

    # Language indicators in class names (e.g., class="language-python")
    LANGUAGE_CLASS_PATTERNS = {
        r'lang(?:uage)?-python': 'python',
        r'lang(?:uage)?-(?:cs|csharp|c#)': 'csharp',
        r'lang(?:uage)?-(?:cpp|c\+\+)': 'cpp',
        r'lang(?:uage)?-(?:js|javascript)': 'javascript',
        r'lang(?:uage)?-(?:ts|typescript)': 'typescript',
        r'lang(?:uage)?-java(?!script)': 'java',
        r'lang(?:uage)?-rust': 'rust',
        r'lang(?:uage)?-go': 'go',
        r'lang(?:uage)?-ruby': 'ruby',
        r'lang(?:uage)?-php': 'php',
    }

    def detect_code_blocks(self, html_content, page_url):
        """
        Detect code blocks in HTML.

        Returns list of code fragments with metadata.
        """
        soup = BeautifulSoup(html_content, 'html.parser')
        fragments = []

        # Find all <pre><code> blocks (most common pattern)
        for pre in soup.find_all('pre'):
            code_tag = pre.find('code')
            if code_tag:
                code_text = code_tag.get_text()

                # Skip empty or very short snippets
                if not code_text or len(code_text.strip()) < 20:
                    continue

                # Detect language from class attribute
                language = self._detect_language(code_tag)

                # Extract context (surrounding headings)
                context = self._extract_context(pre)

                fragments.append({
                    'code': code_text,
                    'language': language,
                    'source_url': page_url,
                    'context_tags': context.get('tags', []),
                    'function_name': context.get('function_name'),
                    'additional_metadata': {
                        'heading': context.get('heading'),
                        'section': context.get('section'),
                    }
                })

        return fragments

    def _detect_language(self, code_tag):
        """Detect programming language from class attribute."""
        class_attr = code_tag.get('class', [])
        if isinstance(class_attr, list):
            class_str = ' '.join(class_attr)
        else:
            class_str = class_attr

        for pattern, lang in self.LANGUAGE_CLASS_PATTERNS.items():
            if re.search(pattern, class_str, re.IGNORECASE):
                return lang

        return None  # Auto-detect

    def _extract_context(self, element):
        """Extract contextual information around code block."""
        context = {
            'tags': [],
            'heading': None,
            'section': None,
            'function_name': None,
        }

        # Find preceding heading
        heading = element.find_previous(['h1', 'h2', 'h3', 'h4'])
        if heading:
            context['heading'] = heading.get_text().strip()

            # Extract keywords from heading as tags
            heading_words = re.findall(r'\b[a-z]{4,}\b', context['heading'].lower())
            context['tags'] = heading_words[:5]  # Limit to 5 tags

        # Try to extract function/method name from code
        code_text = element.get_text()
        func_match = re.search(r'(?:def|function|public|private)\s+(\w+)\s*\(', code_text)
        if func_match:
            context['function_name'] = func_match.group(1)

        return context
```

### Pattern 2: Repository Link Detection

Detect GitHub/GitLab repository links and clone them.

```python
import re
from urllib.parse import urlparse

class RepositoryDetector:
    """Detects repository links in HTML content."""

    REPO_PATTERNS = [
        r'https://github\.com/[\w-]+/[\w.-]+',
        r'https://gitlab\.com/[\w-]+/[\w.-]+',
        r'https://bitbucket\.org/[\w-]+/[\w.-]+',
        r'git@github\.com:[\w-]+/[\w.-]+\.git',
    ]

    def detect_repositories(self, html_content):
        """
        Detect repository URLs in HTML.

        Returns list of unique repository URLs.
        """
        repos = set()

        for pattern in self.REPO_PATTERNS:
            matches = re.findall(pattern, html_content)
            repos.update(matches)

        # Clean up URLs (remove trailing .git, normalize)
        clean_repos = []
        for repo_url in repos:
            # Remove .git suffix
            repo_url = re.sub(r'\.git$', '', repo_url)

            # Convert SSH to HTTPS for consistency
            if repo_url.startswith('git@github.com:'):
                repo_url = repo_url.replace('git@github.com:', 'https://github.com/')

            clean_repos.append(repo_url)

        return clean_repos
```

### Pattern 3: Crawl4AI Hook Integration

Integrate detection into Crawl4AI as a processing hook.

```python
from crawl4ai import AsyncWebCrawler
from crawl4ai.async_crawler_strategy import AsyncPlaywrightCrawlerStrategy

async def process_page_with_cocoindex(url, html_content, mcp_client):
    """
    Process crawled page: detect code and repositories, ingest into CocoIndex.

    Args:
        url: Page URL
        html_content: HTML content of page
        mcp_client: Connected MCP client instance

    Returns:
        dict with ingestion statistics
    """
    code_detector = CodeFragmentDetector()
    repo_detector = RepositoryDetector()

    stats = {
        'code_fragments_found': 0,
        'code_fragments_ingested': 0,
        'repositories_found': 0,
        'repositories_cloned': 0,
        'errors': []
    }

    # Detect and ingest code fragments
    fragments = code_detector.detect_code_blocks(html_content, url)
    stats['code_fragments_found'] = len(fragments)

    for fragment in fragments:
        try:
            result = await mcp_client.call_tool("ingest-code-fragment", fragment)
            if not result.get('isError'):
                stats['code_fragments_ingested'] += 1
            else:
                stats['errors'].append(f"Failed to ingest fragment: {result}")
        except Exception as e:
            stats['errors'].append(f"Error ingesting fragment: {e}")

    # Detect and clone repositories
    repositories = repo_detector.detect_repositories(html_content)
    stats['repositories_found'] = len(repositories)

    for repo_url in repositories:
        try:
            result = await mcp_client.call_tool("clone-and-index-repo", {
                "git_url": repo_url,
                "update_existing": True
            })
            if not result.get('isError'):
                stats['repositories_cloned'] += 1
            else:
                stats['errors'].append(f"Failed to clone {repo_url}: {result}")
        except Exception as e:
            stats['errors'].append(f"Error cloning {repo_url}: {e}")

    return stats


async def crawl_with_code_extraction(urls, mcp_server_host, mcp_server_port):
    """
    Crawl URLs and automatically extract/index code.

    Args:
        urls: List of URLs to crawl
        mcp_server_host: CocoIndex MCP server host
        mcp_server_port: CocoIndex MCP server port

    Returns:
        dict with aggregated statistics
    """
    from tests.mcp_client import MCPTestClient

    total_stats = {
        'pages_crawled': 0,
        'code_fragments_total': 0,
        'repositories_total': 0,
        'errors': []
    }

    async with MCPTestClient(host=mcp_server_host, port=mcp_server_port, transport='http') as mcp_client:
        async with AsyncWebCrawler() as crawler:
            for url in urls:
                try:
                    result = await crawler.arun(url=url)

                    if result.success:
                        # Process page content
                        stats = await process_page_with_cocoindex(
                            url,
                            result.html,
                            mcp_client
                        )

                        total_stats['pages_crawled'] += 1
                        total_stats['code_fragments_total'] += stats['code_fragments_ingested']
                        total_stats['repositories_total'] += stats['repositories_cloned']
                        total_stats['errors'].extend(stats['errors'])

                        print(f"Processed {url}: {stats['code_fragments_ingested']} fragments, {stats['repositories_cloned']} repos")
                    else:
                        total_stats['errors'].append(f"Failed to crawl {url}")

                except Exception as e:
                    total_stats['errors'].append(f"Error crawling {url}: {e}")

    return total_stats
```

### Pattern 4: Selective Crawling with Filters

Only extract code from relevant pages.

```python
async def should_process_page(url, html_content):
    """
    Determine if page should be processed for code extraction.

    Returns True if page likely contains useful code examples.
    """
    # Skip certain URL patterns
    skip_patterns = [
        r'/terms',
        r'/privacy',
        r'/about',
        r'/contact',
        r'/pricing',
        r'/blog/page/\d+',  # Blog pagination
    ]

    for pattern in skip_patterns:
        if re.search(pattern, url, re.IGNORECASE):
            return False

    # Look for code indicators in HTML
    soup = BeautifulSoup(html_content, 'html.parser')

    # Must have code blocks
    code_blocks = soup.find_all(['pre', 'code'])
    if len(code_blocks) < 2:  # At least 2 code blocks
        return False

    # Positive indicators
    positive_keywords = [
        'tutorial', 'example', 'guide', 'documentation',
        'how to', 'sample', 'demo', 'quickstart'
    ]

    page_text = soup.get_text().lower()
    has_positive_indicator = any(keyword in page_text for keyword in positive_keywords)

    return has_positive_indicator
```

## Configuration Example

### crawl4ai_config.yaml

```yaml
cocoindex:
  mcp_server:
    host: "192.168.1.11"
    port: 3033
    transport: "http"

  filtering:
    min_code_length: 20          # Minimum characters in code snippet
    max_code_length: 5000        # Maximum characters in code snippet
    min_code_blocks_per_page: 2  # Minimum code blocks to process page
    skip_url_patterns:
      - "/terms"
      - "/privacy"
      - "/pricing"

  code_detection:
    extract_context: true         # Extract surrounding headings
    auto_detect_language: true    # Auto-detect if class doesn't specify
    extract_function_names: true  # Try to extract function names

  repositories:
    auto_clone: true              # Automatically clone detected repos
    update_existing: true         # Pull updates for existing repos
    subdirectory_only: false      # Index only subdirectory if specified
```

## Best Practices

### 1. Rate Limiting

Don't overwhelm the MCP server:

```python
import asyncio

async def rate_limited_ingestion(fragments, mcp_client, max_per_second=5):
    """Ingest code fragments with rate limiting."""
    for fragment in fragments:
        await mcp_client.call_tool("ingest-code-fragment", fragment)
        await asyncio.sleep(1 / max_per_second)  # Rate limit
```

### 2. Deduplication

Avoid indexing duplicate code:

```python
import hashlib

class CodeDeduplicator:
    """Track ingested code to avoid duplicates."""

    def __init__(self):
        self.seen_hashes = set()

    def is_duplicate(self, code_text):
        """Check if code has already been ingested."""
        code_hash = hashlib.sha256(code_text.encode()).hexdigest()

        if code_hash in self.seen_hashes:
            return True

        self.seen_hashes.add(code_hash)
        return False
```

### 3. Error Handling

Handle ingestion failures gracefully:

```python
async def safe_ingest(fragment, mcp_client, max_retries=3):
    """Ingest with retry logic."""
    for attempt in range(max_retries):
        try:
            result = await mcp_client.call_tool("ingest-code-fragment", fragment)

            if not result.get('isError'):
                return result

            # Error from MCP server
            error_msg = result.get('content', 'Unknown error')
            print(f"Ingestion failed (attempt {attempt + 1}): {error_msg}")

            if attempt < max_retries - 1:
                await asyncio.sleep(2 ** attempt)  # Exponential backoff

        except Exception as e:
            print(f"Exception during ingestion (attempt {attempt + 1}): {e}")
            if attempt < max_retries - 1:
                await asyncio.sleep(2 ** attempt)

    return {'error': 'Max retries exceeded'}
```

## Testing

Test code detection and ingestion:

```python
async def test_code_detection():
    """Test code fragment detection and ingestion."""

    test_html = """
    <html>
    <body>
        <h2>Dependency Injection Example</h2>
        <p>Here's how to configure services in ASP.NET Core:</p>
        <pre><code class="language-csharp">
public void ConfigureServices(IServiceCollection services)
{
    services.AddTransient<IMyService, MyService>();
}
        </code></pre>

        <h2>GitHub Repository</h2>
        <p>Sample code: <a href="https://github.com/dotnet/aspnetcore">ASP.NET Core</a></p>
    </body>
    </html>
    """

    detector = CodeFragmentDetector()
    repo_detector = RepositoryDetector()

    # Detect code
    fragments = detector.detect_code_blocks(test_html, "https://example.com/test")
    print(f"Found {len(fragments)} code fragments")
    for fragment in fragments:
        print(f"  Language: {fragment['language']}, Length: {len(fragment['code'])} chars")

    # Detect repos
    repos = repo_detector.detect_repositories(test_html)
    print(f"Found {len(repos)} repositories: {repos}")

    # Test ingestion
    async with MCPTestClient(host="192.168.1.11", port=3033, transport='http') as client:
        for fragment in fragments:
            result = await client.call_tool("ingest-code-fragment", fragment)
            print(f"Ingestion result: {result}")
```

## Integration with Archon RAG

See [ARCHON_INTEGRATION.md](./ARCHON_INTEGRATION.md) for detailed guidance on integrating with the Archon RAG tool.

## Summary

Crawl4AI can automatically populate CocoIndex by:
- Detecting code snippets in documentation and tutorials
- Extracting metadata and context
- Ingesting via `ingest-code-fragment` tool
- Detecting repository links
- Cloning and indexing via `clone-and-index-repo` tool

This creates a continuously growing, searchable code knowledge base from web crawling activities.
