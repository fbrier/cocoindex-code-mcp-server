#!/usr/bin/env python3
"""
Unit tests for verifying embedding insertion into PostgreSQL with pgvector.

Tests that Python lists are correctly serialized to pgvector format,
while numpy arrays would cause type errors.
"""

import os
from typing import List

import numpy as np
import psycopg
import pytest
from numpy.typing import NDArray
from pgvector.psycopg import register_vector


@pytest.fixture
def db_connection():
    """
    Create a PostgreSQL connection with pgvector support.

    Uses environment variables or defaults to test database on solar.office.multideck.com.
    """
    # Get connection string from environment or use test database
    db_host = os.getenv("COCOINDEX_TEST_DB_HOST", "solar.office.multideck.com")
    db_port = os.getenv("COCOINDEX_TEST_DB_PORT", "5532")
    db_name = os.getenv("COCOINDEX_TEST_DB_NAME", "cocoindex")
    db_user = os.getenv("COCOINDEX_TEST_DB_USER", "cocoindex")
    db_pass = os.getenv("COCOINDEX_TEST_DB_PASSWORD", "cocoindex")

    conn_string = f"host={db_host} port={db_port} dbname={db_name} user={db_user} password={db_pass}"

    conn = psycopg.connect(conn_string)
    # Register pgvector type for this connection
    register_vector(conn)

    yield conn

    conn.close()


@pytest.fixture
def test_table(db_connection):
    """
    Create a test table with vector column for embedding tests.

    Cleans up after test completes.
    """
    table_name = "test_embeddings_insertion"

    with db_connection.cursor() as cur:
        # Drop table if exists (from previous test runs)
        cur.execute(f"DROP TABLE IF EXISTS {table_name}")

        # Create test table with 768-dimensional vector (UniXcoder size)
        cur.execute(
            f"""
            CREATE TABLE {table_name} (
                id SERIAL PRIMARY KEY,
                content TEXT NOT NULL,
                embedding vector(768) NOT NULL,
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
        """
        )
        db_connection.commit()

    yield table_name

    # Cleanup
    with db_connection.cursor() as cur:
        cur.execute(f"DROP TABLE IF EXISTS {table_name}")
        db_connection.commit()


def test_python_list_embedding_insertion(db_connection, test_table):
    """
    Test that Python list embeddings insert successfully into pgvector column.

    This is the CORRECT format that our embedding functions should return.
    """
    # Create a 768-dimensional embedding as Python list
    embedding_list: List[float] = [0.1 * i for i in range(768)]

    with db_connection.cursor() as cur:
        cur.execute(
            f"""
            INSERT INTO {test_table} (content, embedding)
            VALUES (%s, %s)
            RETURNING id
        """,
            ("test content", embedding_list),
        )
        inserted_id = cur.fetchone()[0]
        db_connection.commit()

    assert inserted_id is not None, "Failed to insert embedding as Python list"

    # Verify the embedding was stored correctly
    with db_connection.cursor() as cur:
        cur.execute(f"SELECT embedding FROM {test_table} WHERE id = %s", (inserted_id,))
        retrieved_embedding = cur.fetchone()[0]

    # pgvector returns embeddings as numpy arrays when register_vector() is called
    # (which is correct behavior - the important part is that insertion succeeded)
    assert retrieved_embedding is not None, "Failed to retrieve embedding"
    assert len(retrieved_embedding) == 768, f"Expected 768 dimensions, got {len(retrieved_embedding)}"

    # Verify values are correct (with float precision tolerance for float32)
    for i, val in enumerate(retrieved_embedding):
        expected = 0.1 * i
        assert abs(val - expected) < 1e-5, f"Value mismatch at index {i}: {val} != {expected}"


def test_numpy_array_embedding_without_register_fails(test_table):
    """
    Test that numpy arrays FAIL when pgvector adapters are not registered.

    This simulates CocoIndex's PostgreSQL target behavior - it doesn't register
    pgvector adapters, so numpy arrays get serialized as JSON and cause type errors.
    """
    # Create connection WITHOUT register_vector() - simulates CocoIndex behavior
    db_host = os.getenv("COCOINDEX_TEST_DB_HOST", "solar.office.multideck.com")
    db_port = os.getenv("COCOINDEX_TEST_DB_PORT", "5532")
    db_name = os.getenv("COCOINDEX_TEST_DB_NAME", "cocoindex")
    db_user = os.getenv("COCOINDEX_TEST_DB_USER", "cocoindex")
    db_pass = os.getenv("COCOINDEX_TEST_DB_PASSWORD", "cocoindex")

    conn_string = f"host={db_host} port={db_port} dbname={db_name} user={db_user} password={db_pass}"
    conn = psycopg.connect(conn_string)
    # NOTE: Deliberately NOT calling register_vector(conn)

    try:
        # Create a 768-dimensional embedding as numpy array
        embedding_array: NDArray[np.float32] = np.array([0.1 * i for i in range(768)], dtype=np.float32)

        with conn.cursor() as cur:
            # This should FAIL - numpy arrays cannot be adapted without register_vector()
            # Can raise either ProgrammingError (cannot adapt) or DatatypeMismatch (jsonb/vector)
            with pytest.raises((psycopg.errors.ProgrammingError, psycopg.errors.DatatypeMismatch)) as exc_info:
                cur.execute(
                    f"""
                    INSERT INTO {test_table} (content, embedding)
                    VALUES (%s, %s)
                """,
                    ("test content", embedding_array),
                )
                conn.commit()

            # Verify the error message indicates adapter/type issue
            error_msg = str(exc_info.value).lower()
            assert "adapt" in error_msg or "vector" in error_msg or "jsonb" in error_msg, (
                f"Expected adapter or type error, got: {error_msg}"
            )

    finally:
        conn.close()


def test_numpy_to_list_conversion():
    """
    Test that numpy arrays can be converted to Python lists for pgvector compatibility.

    This verifies the conversion logic used in safe_embed_with_retry().
    """
    # Create numpy array embedding
    embedding_array: NDArray[np.float32] = np.array([0.1 * i for i in range(768)], dtype=np.float32)

    # Convert to Python list (what our embedding functions do)
    embedding_list: List[float] = embedding_array.tolist()

    # Verify conversion
    assert isinstance(embedding_list, list), f"Expected list, got {type(embedding_list)}"
    assert len(embedding_list) == 768, f"Expected 768 dimensions, got {len(embedding_list)}"

    # Verify values are preserved (with float32 precision tolerance)
    for i, val in enumerate(embedding_list):
        expected = 0.1 * i
        assert abs(val - expected) < 1e-5, f"Value mismatch at index {i}: {val} != {expected}"


def test_embedding_similarity_search(db_connection, test_table):
    """
    Test that vector similarity search works with Python list embeddings.

    Verifies end-to-end workflow: insert embeddings, perform similarity search.
    """
    # Insert multiple test embeddings
    test_data = [
        ("apple fruit", [1.0 if i < 100 else 0.0 for i in range(768)]),
        ("banana fruit", [1.0 if 50 < i < 150 else 0.0 for i in range(768)]),
        ("car vehicle", [1.0 if 400 < i < 500 else 0.0 for i in range(768)]),
    ]

    with db_connection.cursor() as cur:
        for content, embedding in test_data:
            cur.execute(
                f"""
                INSERT INTO {test_table} (content, embedding)
                VALUES (%s, %s)
            """,
                (content, embedding),
            )
        db_connection.commit()

    # Search for similar to "apple" (first 100 dims = 1.0)
    query_embedding = [1.0 if i < 100 else 0.0 for i in range(768)]

    with db_connection.cursor() as cur:
        cur.execute(
            f"""
            SELECT content, embedding <-> %s::vector AS distance
            FROM {test_table}
            ORDER BY distance
            LIMIT 3
        """,
            (query_embedding,),
        )
        results = cur.fetchall()

    # Verify results are ordered by similarity
    assert len(results) == 3, f"Expected 3 results, got {len(results)}"
    assert results[0][0] == "apple fruit", f"Most similar should be 'apple fruit', got {results[0][0]}"
    assert results[0][1] < results[1][1], "Distances should be ascending"
    assert results[1][1] < results[2][1], "Distances should be ascending"


if __name__ == "__main__":
    # Allow running tests directly
    pytest.main([__file__, "-v"])
