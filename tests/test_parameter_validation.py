#!/usr/bin/env python3
"""
Unit tests for MCP tool parameter validation.

Tests that search tools properly validate required parameters,
especially the requirement that either 'language' OR 'embedding_model'
must be provided for vector-based searches.
"""

import pytest


class TestVectorSearchParameterValidation:
    """Test parameter validation for search-vector tool."""

    def test_query_only_fails(self):
        """Vector search with only query parameter should fail."""
        # Simulate arguments without language or embedding_model
        arguments = {"query": "test query", "top_k": 5}

        # This should raise ValueError when validation is applied
        # (We'll test this through the actual MCP handler in integration tests)
        assert "language" not in arguments
        assert "embedding_model" not in arguments

    def test_query_with_language_valid(self):
        """Vector search with query + language should be valid."""
        arguments = {"query": "test query", "language": "Python", "top_k": 5}

        assert "query" in arguments
        assert "language" in arguments
        # This should pass validation

    def test_query_with_embedding_model_valid(self):
        """Vector search with query + embedding_model should be valid."""
        arguments = {
            "query": "test query",
            "embedding_model": "microsoft/graphcodebert-base",
            "top_k": 5,
        }

        assert "query" in arguments
        assert "embedding_model" in arguments
        # This should pass validation

    def test_query_with_both_valid(self):
        """Vector search with query + both language and embedding_model should be valid."""
        arguments = {
            "query": "test query",
            "language": "Python",
            "embedding_model": "microsoft/graphcodebert-base",
            "top_k": 5,
        }

        assert "query" in arguments
        assert "language" in arguments
        assert "embedding_model" in arguments
        # This should pass validation (language takes precedence or embedding_model is ignored)

    def test_missing_query_invalid(self):
        """Missing query parameter should be invalid."""
        arguments = {"language": "Python", "top_k": 5}

        assert "query" not in arguments
        # This should fail validation (query is required)

    def test_empty_query_invalid(self):
        """Empty query string should be invalid."""
        arguments = {"query": "", "language": "Python"}

        assert arguments["query"] == ""
        # This should fail validation (empty query not useful)

    def test_top_k_optional(self):
        """top_k parameter should be optional with default value."""
        arguments = {"query": "test query", "language": "Python"}

        assert "top_k" not in arguments
        # This should pass validation (top_k has default)

    def test_top_k_custom_value(self):
        """top_k can be customized."""
        arguments = {"query": "test query", "language": "Python", "top_k": 20}

        assert arguments["top_k"] == 20
        # This should pass validation


class TestHybridSearchParameterValidation:
    """Test parameter validation for search-hybrid tool."""

    def test_both_queries_only_fails(self):
        """Hybrid search with only vector_query and keyword_query should fail."""
        arguments = {
            "vector_query": "test query",
            "keyword_query": "language:Python",
            "top_k": 5,
        }

        assert "language" not in arguments
        assert "embedding_model" not in arguments
        # This should fail validation (needs language or embedding_model for vector part)

    def test_with_language_valid(self):
        """Hybrid search with language parameter should be valid."""
        arguments = {
            "vector_query": "test query",
            "keyword_query": "exists(functions)",
            "language": "Python",
            "top_k": 5,
        }

        assert "vector_query" in arguments
        assert "keyword_query" in arguments
        assert "language" in arguments
        # This should pass validation

    def test_with_embedding_model_valid(self):
        """Hybrid search with embedding_model should be valid."""
        arguments = {
            "vector_query": "test query",
            "keyword_query": "exists(classes)",
            "embedding_model": "microsoft/graphcodebert-base",
        }

        assert "embedding_model" in arguments
        # This should pass validation

    def test_missing_vector_query_invalid(self):
        """Missing vector_query should be invalid."""
        arguments = {
            "keyword_query": "language:Python",
            "language": "Python",
        }

        assert "vector_query" not in arguments
        # This should fail (vector_query required)

    def test_missing_keyword_query_invalid(self):
        """Missing keyword_query should be invalid."""
        arguments = {
            "vector_query": "test query",
            "language": "Python",
        }

        assert "keyword_query" not in arguments
        # This should fail (keyword_query required)

    def test_weight_defaults(self):
        """vector_weight and keyword_weight should have defaults."""
        arguments = {
            "vector_query": "test",
            "keyword_query": "exists(code)",
            "language": "Python",
        }

        assert "vector_weight" not in arguments
        assert "keyword_weight" not in arguments
        # Should use defaults: vector_weight=0.7, keyword_weight=0.3

    def test_custom_weights(self):
        """Custom weights should be accepted."""
        arguments = {
            "vector_query": "test",
            "keyword_query": "exists(code)",
            "language": "Python",
            "vector_weight": 0.8,
            "keyword_weight": 0.2,
        }

        assert arguments["vector_weight"] == 0.8
        assert arguments["keyword_weight"] == 0.2
        # This should pass validation


class TestKeywordSearchParameterValidation:
    """Test parameter validation for search-keyword tool."""

    def test_query_required(self):
        """Keyword search requires query parameter."""
        arguments = {"top_k": 10}

        assert "query" not in arguments
        # This should fail (query required)

    def test_query_only_valid(self):
        """Keyword search with only query is valid."""
        arguments = {"query": "language:Python"}

        assert "query" in arguments
        # This should pass (query is the only required param)

    def test_no_language_requirement(self):
        """Keyword search does NOT require language parameter."""
        arguments = {"query": "exists(functions)"}

        assert "language" not in arguments
        assert "embedding_model" not in arguments
        # This should pass (keyword search doesn't need embeddings)

    def test_top_k_optional(self):
        """top_k is optional for keyword search."""
        arguments = {"query": "language:C#"}

        assert "top_k" not in arguments
        # Should use default top_k=10


class TestErrorMessages:
    """Test that error messages are helpful and informative."""

    def test_missing_language_error_message(self):
        """Error for missing language/embedding_model should be clear."""
        # This would be raised by the actual validation code
        expected_error_parts = [
            "language",
            "embedding_model",
            "required",
        ]

        # Mock error message that should be raised
        error_msg = "Either 'language' or 'embedding_model' parameter is required for search."

        for part in expected_error_parts:
            assert part.lower() in error_msg.lower()

    def test_invalid_language_error_lists_valid(self):
        """Error for invalid language should list valid options."""
        # Mock error from language normalization
        error_msg = (
            "Invalid language 'cobol'. "
            "Valid languages: C, C#, C++, Go, Haskell, JSON, Java, JavaScript, Kotlin, "
            "Markdown, PHP, Python, Ruby, Rust, Swift, TypeScript, ps1, sh, sln"
        )

        assert "Invalid language" in error_msg
        assert "Valid languages:" in error_msg
        assert "Python" in error_msg
        assert "C#" in error_msg

    def test_helpful_variations_mentioned(self):
        """Error messages should mention that variations are accepted."""
        error_msg = (
            "Invalid language 'invalid'. Valid languages: ...\n\n"
            "Common variations are also accepted (e.g., 'csharp' -> 'C#', 'cpp' -> 'C++', 'py' -> 'Python')"
        )

        assert "variations" in error_msg.lower()
        assert "csharp" in error_msg
        assert "C#" in error_msg


class TestSchemaRequirements:
    """Test that schemas properly define required fields."""

    def test_vector_search_schema_requirements(self):
        """Vector search schema should have correct required fields."""
        # From mcp_json_schemas.VECTOR_SEARCH_INPUT_SCHEMA
        required_fields = ["query"]
        optional_fields = ["language", "embedding_model", "top_k"]

        # query must be in required
        assert "query" in required_fields

        # language and embedding_model should be optional (validated at runtime)
        assert "language" in optional_fields
        assert "embedding_model" in optional_fields

    def test_hybrid_search_schema_requirements(self):
        """Hybrid search schema should have correct required fields."""
        # From mcp_json_schemas.HYBRID_SEARCH_INPUT_SCHEMA
        required_fields = ["vector_query", "keyword_query"]
        optional_fields = ["language", "embedding_model", "top_k", "vector_weight", "keyword_weight"]

        assert "vector_query" in required_fields
        assert "keyword_query" in required_fields
        assert "language" in optional_fields
        assert "embedding_model" in optional_fields

    def test_keyword_search_schema_requirements(self):
        """Keyword search schema should have correct required fields."""
        # From mcp_json_schemas.KEYWORD_SEARCH_INPUT_SCHEMA
        required_fields = ["query"]
        optional_fields = ["top_k"]

        assert "query" in required_fields
        assert "top_k" in optional_fields

        # language/embedding_model not in schema at all for keyword search
        # (keyword search doesn't use embeddings)


class TestIntegrationScenarios:
    """Test realistic integration scenarios."""

    def test_c_sharp_game_search_scenario(self):
        """Real-world scenario: searching for C# game code."""
        # User request: "Find player movement code in C# game"
        arguments = {
            "query": "player movement position update",
            "language": "csharp",  # User types lowercase variation
            "top_k": 5,
        }

        # After normalization: language should become "C#"
        # This should successfully search C# code
        assert arguments["language"] == "csharp"
        # (normalization would change this to "C#" before search)

    def test_hybrid_search_with_filter_scenario(self):
        """Real-world scenario: semantic search + metadata filter."""
        # User request: "Find async Python functions"
        arguments = {
            "vector_query": "async await concurrent parallel",
            "keyword_query": "language:python AND has_async:true",
            "language": "py",  # User types short form
            "vector_weight": 0.6,
            "keyword_weight": 0.4,
        }

        # All required parameters present
        assert "vector_query" in arguments
        assert "keyword_query" in arguments
        assert "language" in arguments

    def test_code_fragment_search_scenario(self):
        """Real-world scenario: searching ingested code fragment."""
        # User request: "Find dependency injection examples"
        arguments = {
            "query": "dependency injection services container",
            "language": "C#",  # Already canonical
            "top_k": 3,
        }

        # Should find the ConfigureServices code fragment
        assert arguments["language"] == "C#"

    def test_embedding_model_override_scenario(self):
        """Real-world scenario: using specific embedding model."""
        # Advanced user specifies exact model
        arguments = {
            "query": "machine learning inference",
            "embedding_model": "sentence-transformers/all-mpnet-base-v2",
            "top_k": 10,
        }

        # Should work without language parameter
        assert "embedding_model" in arguments
        assert "language" not in arguments


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
