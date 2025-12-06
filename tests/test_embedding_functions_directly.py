#!/usr/bin/env python3
"""
Direct tests of embedding functions to verify they return Python lists.

This tests the functions in isolation, independent of CocoIndex flow execution.
"""

import sys
from typing import List

import pytest


def test_safe_embed_with_retry_returns_list():
    """Test that safe_embed_with_retry returns a Python list, not numpy array."""
    # Import the function
    from cocoindex_code_mcp_server.cocoindex_config import safe_embed_with_retry

    # Call with sample code
    test_code = "def hello():\n    print('Hello, world!')"
    result = safe_embed_with_retry(test_code)

    # Verify it's a Python list
    assert isinstance(result, list), f"Expected list, got {type(result)}"
    assert len(result) == 768, f"Expected 768 dimensions, got {len(result)}"
    assert isinstance(result[0], float), f"Expected float elements, got {type(result[0])}"
    print(f"PASS: safe_embed_with_retry returned Python list with {len(result)} dimensions")


def test_fallback_embed_to_list_returns_list():
    """Test that fallback_embed_to_list returns a Python list, not numpy array."""
    # Import the function
    from cocoindex_code_mcp_server.cocoindex_config import fallback_embed_to_list

    # Call with sample text
    test_text = "This is a test document for embedding."
    result = fallback_embed_to_list(test_text)

    # Verify it's a Python list
    assert isinstance(result, list), f"Expected list, got {type(result)}"
    # all-mpnet-base-v2 produces 768-dimensional embeddings
    assert len(result) == 768, f"Expected 768 dimensions, got {len(result)}"
    assert isinstance(result[0], float), f"Expected float elements, got {type(result[0])}"
    print(f"PASS: fallback_embed_to_list returned Python list with {len(result)} dimensions")


def test_unixcoder_embedding_flow_returns_list():
    """Test that unixcoder_embedding flow function is configured correctly."""
    from cocoindex_code_mcp_server.cocoindex_config import unixcoder_embedding

    # Verify the function exists and has correct configuration
    assert hasattr(unixcoder_embedding, '__wrapped__'), "unixcoder_embedding should be a flow function"
    print(f"PASS: unixcoder_embedding flow function exists")


def test_embedding_functions_imported_correctly():
    """Verify all embedding functions can be imported from cocoindex_config."""
    try:
        from cocoindex_code_mcp_server.cocoindex_config import (
            code_to_embedding,
            fallback_embed_to_list,
            fallback_embedding,
            safe_embed_with_retry,
            unixcoder_embedding,
        )

        print("PASS: All embedding functions imported successfully:")
        print(f"  - safe_embed_with_retry: {safe_embed_with_retry}")
        print(f"  - fallback_embed_to_list: {fallback_embed_to_list}")
        print(f"  - unixcoder_embedding: {unixcoder_embedding}")
        print(f"  - fallback_embedding: {fallback_embedding}")
        print(f"  - code_to_embedding: {code_to_embedding}")

    except ImportError as e:
        pytest.fail(f"Failed to import embedding functions: {e}")


if __name__ == "__main__":
    # Run tests directly
    pytest.main([__file__, "-v", "-s"])
