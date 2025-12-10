#!/usr/bin/env python3
"""
Integration test that replicates the EXACT CocoIndex flow execution error.

This test uses CocoIndex's Rust execution engine to process files and insert
embeddings into PostgreSQL, replicating the production behavior.

Expected to FAIL with: "column embedding is of type vector but expression is of type jsonb"
when embedding functions return Python lists instead of strings.
"""

import os
import tempfile
from pathlib import Path

import cocoindex
import psycopg
import pytest
from dotenv import load_dotenv


@pytest.fixture(scope="module")
def setup_cocoindex():
    """
    Initialize CocoIndex and database.

    Uses environment variables with defaults:
    - CI/GitHub Actions: localhost:5432 (PostgreSQL service container)
    - Local development: solar.office.multideck.com:5532 (intranet server)
    """
    load_dotenv()

    # Get database URL (defaults for LOCAL testing, GitHub Actions overrides)
    # Defaults: solar.office.multideck.com:5532 (local/intranet)
    # CI overrides: localhost:5432 (GitHub Actions service container)
    db_host = os.getenv("COCOINDEX_TEST_DB_HOST", "solar.office.multideck.com")
    db_port = os.getenv("COCOINDEX_TEST_DB_PORT", "5532")
    db_name = os.getenv("COCOINDEX_TEST_DB_NAME", "cocoindex")
    db_user = os.getenv("COCOINDEX_TEST_DB_USER", "cocoindex")
    db_pass = os.getenv("COCOINDEX_TEST_DB_PASSWORD", "cocoindex")

    database_url = f"postgresql://{db_user}:{db_pass}@{db_host}:{db_port}/{db_name}"

    # CRITICAL: Set environment variable BEFORE cocoindex.init()
    # CocoIndex requires COCOINDEX_DATABASE_URL to be set for flow execution
    os.environ["COCOINDEX_DATABASE_URL"] = database_url

    # Now initialize CocoIndex with database URL set
    cocoindex.init()

    yield database_url


@pytest.fixture
def test_flow_tables(setup_cocoindex):
    """Create and cleanup test flow tables."""
    database_url = setup_cocoindex

    # Table names for our test flow
    embeddings_table = "test_flow_embeddings"
    tracking_table = "test_flow__cocoindex_tracking"

    conn = psycopg.connect(database_url)
    cur = conn.cursor()

    # Drop existing tables if they exist
    cur.execute(f"DROP TABLE IF EXISTS {embeddings_table} CASCADE")
    cur.execute(f"DROP TABLE IF EXISTS {tracking_table} CASCADE")
    conn.commit()

    yield embeddings_table, tracking_table, database_url

    # Cleanup
    cur.execute(f"DROP TABLE IF EXISTS {embeddings_table} CASCADE")
    cur.execute(f"DROP TABLE IF EXISTS {tracking_table} CASCADE")
    conn.commit()
    cur.close()
    conn.close()


@pytest.fixture
def test_code_directory():
    """Create a temporary directory with a test code file."""
    # Create temp directory
    temp_dir = tempfile.mkdtemp()

    # Create test file in the directory
    test_file = os.path.join(temp_dir, "TestClass.cs")
    with open(test_file, 'w') as f:
        f.write("""
using System;

public class TestClass
{
    public void TestMethod()
    {
        Console.WriteLine("Hello World");
    }
}
""")

    yield temp_dir

    # Cleanup
    try:
        os.unlink(test_file)
        os.rmdir(temp_dir)
    except:
        pass


def test_cocoindex_flow_with_string_embeddings_AFTER_FIX(test_flow_tables, test_code_directory):
    """
    Test CocoIndex flow execution with string embeddings (AFTER fix).

    This test uses the ACTUAL production code_embedding_flow to process a file
    and insert embeddings. This replicates EXACTLY what the MCP server does.
    """
    embeddings_table, tracking_table, database_url = test_flow_tables

    # Import the ACTUAL production flow and embedding function
    from cocoindex_code_mcp_server.cocoindex_config import (
        _global_flow_config,
        code_embedding_flow,
        safe_embed_with_retry,
    )

    # Verify embedding function returns string (AFTER fix)
    test_embedding = safe_embed_with_retry("test code")
    assert isinstance(test_embedding, str), "AFTER fix returns string"
    # String representation should look like: "[0.1, 0.2, ...]"
    assert test_embedding.startswith("[") and test_embedding.endswith("]"), "String should be list representation"

    # Configure the production flow EXACTLY like the MCP server does
    # Point to the directory containing the test file
    _global_flow_config["paths"] = [test_code_directory]
    _global_flow_config["use_smart_embedding"] = True  # Use UniXcoder embeddings
    _global_flow_config["use_default_chunking"] = False  # Use AST chunking

    # Note: Database URL already set by setup_cocoindex fixture

    print(f"\n=== Running ACTUAL production flow (AFTER FIX - should succeed) ===")
    print(f"Test directory: {test_code_directory}")
    print(f"Embedding function returns: {type(test_embedding)}")
    print(f"Expected: Flow succeeds with string embeddings")

    # Set up the flow - EXACTLY like MCP server does
    code_embedding_flow.setup()

    # Execute the flow - EXACTLY like MCP server does
    # With string embeddings, this should succeed without errors
    stats = code_embedding_flow.update()

    print(f"\nFlow completed with stats: {stats}")

    # Check if the flow succeeded (no failures)
    stats_str = str(stats)
    if "FAILED" in stats_str:
        print(f"\nFAILURE: Flow had failures with string embeddings!")
        print(f"Stats show failures: {stats}")
        print(f"This means the fix didn't work as expected")
        pytest.fail(f"Flow failed with string embeddings: {stats}")
    else:
        # Flow succeeded - this is the expected result with AFTER fix
        print(f"\nSUCCESS: Flow succeeded with string embeddings!")
        print(f"Stats: {stats}")
        print(f"This PROVES the fix works correctly")
        print(f"String embeddings can be inserted into PostgreSQL vector columns")


if __name__ == "__main__":
    pytest.main([__file__, "-v", "-s"])
