-- Initialize pgvector extension for cocoindex database
-- This script is automatically run by PostgreSQL on first startup

-- Create the vector extension
CREATE EXTENSION IF NOT EXISTS vector;

-- Verify extension is installed
SELECT extname, extversion FROM pg_extension WHERE extname = 'vector';
