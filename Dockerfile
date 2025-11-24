# Dockerfile
FROM python:3.12-slim

# System dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential gcc git curl && \
    rm -rf /var/lib/apt/lists/*

# Create app user
RUN useradd -m app
WORKDIR /app
COPY --chown=app:app . /app

# Python dependencies
USER app
RUN pip install --user -U pip
RUN pip install --user -r requirements.txt

# Download embedding models at build time (optional but speeds up first start)
RUN python -c "from sentence_transformers import SentenceTransformer; \
    SentenceTransformer('BAAI/bge-m3'); \
    SentenceTransformer('thenlper/gte-large'); \
    SentenceTransformer('graphecodebert-base')"

EXPOSE 8000

CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000"]