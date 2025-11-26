# Dockerfile for cocoindex-code-mcp-server with git and repository management support
FROM python:3.12-slim

# System dependencies - includes git for repository cloning
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    gcc \
    git \
    curl \
    ca-certificates \
    openssh-client \
    && rm -rf /var/lib/apt/lists/*

# Create app user
RUN useradd -m -u 1000 app

# Create directories for repos, code fragments, logs, and ssh with proper permissions
RUN mkdir -p /repos /code_fragments /ssh /logs && \
    chown -R app:app /repos /code_fragments /ssh /logs

# Copy init SQL script for pgvector extension
COPY --chown=app:app init-pgvector.sql /app/init-pgvector.sql

WORKDIR /app

# Copy application code
COPY --chown=app:app . /app

# Switch to app user for installation
USER app

# Add user's local bin to PATH
ENV PATH="/home/app/.local/bin:${PATH}"

# Add Python module to PYTHONPATH so it can be imported without installation
ENV PYTHONPATH="/app/python:${PYTHONPATH}"

# Install Python dependencies into virtual environment
# All required languages (C#, C++, C, Python, JS, TS, Java) use tree-sitter Python packages
RUN pip install --user --no-cache-dir --upgrade pip && \
    pip install --user --no-cache-dir uv && \
    uv sync --all-extras

# Expose MCP server port
EXPOSE 3033

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
    CMD curl -f http://localhost:3033/health || exit 1

# Environment variables (can be overridden)
ENV REPOS_DIR=/repos \
    CODE_FRAGMENTS_DIR=/code_fragments \
    GIT_SSH_KEY=/ssh/id_rsa \
    WORKSPACE=/logs

# Run the MCP server using uv run (activates virtual environment)
# Scans both /repos and /code_fragments directories
CMD ["uv", "run", "python", "-m", "cocoindex_code_mcp_server.main_mcp_server", \
     "--port", "3033", \
     "--rescan", \
     "/repos", \
     "/code_fragments"]
