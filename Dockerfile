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

# Accept HuggingFace token as build argument for model downloads
ARG HF_TOKEN
ENV HF_TOKEN=${HF_TOKEN}

# Pre-download embedding models to avoid runtime downloads and rate limiting
# This caches models in the Docker image so they're available offline
# Uses HF_TOKEN if provided to bypass rate limiting
RUN python -c "\
import os; \
from sentence_transformers import SentenceTransformer; \
hf_token = os.environ.get('HF_TOKEN'); \
print('HF_TOKEN status:', 'provided' if hf_token else 'not provided'); \
print('Attempting to download microsoft/unixcoder-base...'); \
try: \
    SentenceTransformer('microsoft/unixcoder-base', token=hf_token); \
    print('[OK] microsoft/unixcoder-base cached'); \
except Exception as e: \
    print('[WARN] Could not download microsoft/unixcoder-base:', str(e)[:100]); \
try: \
    SentenceTransformer('sentence-transformers/all-mpnet-base-v2', token=hf_token); \
    print('[OK] all-mpnet-base-v2 cached'); \
except Exception as e: \
    print('[WARN] Could not download all-mpnet-base-v2:', str(e)[:100]); \
print('Build complete - models will download at runtime if needed'); \
" || true

# Unset HF_TOKEN after model download (security best practice)
# It will be provided again at runtime via docker-compose environment variables
ENV HF_TOKEN=

# Expose MCP server port
EXPOSE 3033

# Environment variables (can be overridden)
ENV REPOS_DIR=/repos \
    CODE_FRAGMENTS_DIR=/code_fragments \
    GIT_SSH_KEY=/ssh/id_rsa \
    WORKSPACE=/logs

# Run the MCP server using uv run (activates virtual environment)
# Scans both /repos and /code_fragments directories
CMD ["uv", "run", "python", "-m", "cocoindex_code_mcp_server.main_mcp_server", \
     "--rescan", \
     "--port", "3033", \
     "/repos", \
     "/code_fragments"]
