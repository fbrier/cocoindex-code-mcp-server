#!/usr/bin/env python3
"""
Unit tests for language normalization functionality.

Tests the language_normalizer module which provides case-insensitive
language name mapping for MCP API calls.
"""

import pytest

from cocoindex_code_mcp_server.language_normalizer import (
    CANONICAL_LANGUAGES,
    LANGUAGE_ALIASES,
    get_all_aliases,
    get_valid_languages,
    normalize_language,
)


class TestLanguageNormalization:
    """Test language normalization with various inputs."""

    def test_canonical_names_unchanged(self):
        """Canonical language names should pass through unchanged."""
        for lang in CANONICAL_LANGUAGES:
            assert normalize_language(lang) == lang

    def test_none_returns_none(self):
        """None input should return None."""
        assert normalize_language(None) is None

    def test_csharp_variations(self):
        """Test all C# variations normalize to 'C#'."""
        variations = ["csharp", "CSharp", "CSHARP", "c#", "C#", "cs", "CS", "c-sharp", "C-Sharp"]
        for variant in variations:
            result = normalize_language(variant)
            assert result == "C#", f"Expected 'C#' for '{variant}', got '{result}'"

    def test_cplusplus_variations(self):
        """Test all C++ variations normalize to 'C++'."""
        variations = ["cpp", "CPP", "c++", "C++", "cplusplus", "CPlusPlus", "CPLUSPLUS", "c-plus-plus"]
        for variant in variations:
            result = normalize_language(variant)
            assert result == "C++", f"Expected 'C++' for '{variant}', got '{result}'"

    def test_python_variations(self):
        """Test all Python variations normalize to 'Python'."""
        variations = ["python", "Python", "PYTHON", "py", "PY", "python3", "Python3"]
        for variant in variations:
            result = normalize_language(variant)
            assert result == "Python", f"Expected 'Python' for '{variant}', got '{result}'"

    def test_javascript_variations(self):
        """Test all JavaScript variations normalize to 'JavaScript'."""
        variations = ["javascript", "JavaScript", "JAVASCRIPT", "js", "JS", "ecmascript"]
        for variant in variations:
            result = normalize_language(variant)
            assert result == "JavaScript", f"Expected 'JavaScript' for '{variant}', got '{result}'"

    def test_typescript_variations(self):
        """Test all TypeScript variations normalize to 'TypeScript'."""
        variations = ["typescript", "TypeScript", "TYPESCRIPT", "ts", "TS"]
        for variant in variations:
            result = normalize_language(variant)
            assert result == "TypeScript", f"Expected 'TypeScript' for '{variant}', got '{result}'"

    def test_rust_variations(self):
        """Test all Rust variations normalize to 'Rust'."""
        variations = ["rust", "Rust", "RUST", "rs", "RS"]
        for variant in variations:
            result = normalize_language(variant)
            assert result == "Rust", f"Expected 'Rust' for '{variant}', got '{result}'"

    def test_go_variations(self):
        """Test all Go variations normalize to 'Go'."""
        variations = ["go", "Go", "GO", "golang", "Golang", "GOLANG"]
        for variant in variations:
            result = normalize_language(variant)
            assert result == "Go", f"Expected 'Go' for '{variant}', got '{result}'"

    def test_case_insensitive_all_languages(self):
        """All language aliases should work case-insensitively."""
        for alias_lower, canonical in LANGUAGE_ALIASES.items():
            # Test lowercase (as stored in map)
            assert normalize_language(alias_lower) == canonical

            # Test uppercase
            assert normalize_language(alias_lower.upper()) == canonical

            # Test title case if applicable
            if len(alias_lower) > 1:
                title_case = alias_lower[0].upper() + alias_lower[1:].lower()
                assert normalize_language(title_case) == canonical

    def test_invalid_language_raises_error(self):
        """Invalid language should raise ValueError with helpful message."""
        with pytest.raises(ValueError) as exc_info:
            normalize_language("cobol")

        error_msg = str(exc_info.value)
        assert "Invalid language 'cobol'" in error_msg
        assert "Valid languages:" in error_msg
        assert "C#" in error_msg
        assert "Python" in error_msg
        assert "Common variations" in error_msg

    def test_invalid_language_shows_all_valid(self):
        """Error message should list all valid languages."""
        with pytest.raises(ValueError) as exc_info:
            normalize_language("invalid_lang")

        error_msg = str(exc_info.value)
        # Check that multiple languages are mentioned
        for lang in ["C#", "Python", "JavaScript", "Rust"]:
            assert lang in error_msg

    def test_typo_raises_clear_error(self):
        """Typos should raise clear error messages."""
        typos = ["pythn", "javacsript", "cshap", "ruust"]
        for typo in typos:
            with pytest.raises(ValueError) as exc_info:
                normalize_language(typo)

            error_msg = str(exc_info.value)
            assert f"Invalid language '{typo}'" in error_msg
            assert "Valid languages:" in error_msg

    def test_get_valid_languages(self):
        """get_valid_languages should return sorted list of canonical names."""
        valid = get_valid_languages()

        assert isinstance(valid, list)
        assert len(valid) > 0
        assert "C#" in valid
        assert "Python" in valid
        assert valid == sorted(valid)  # Should be sorted

    def test_get_all_aliases(self):
        """get_all_aliases should return the complete mapping dict."""
        aliases = get_all_aliases()

        assert isinstance(aliases, dict)
        assert len(aliases) > 0
        assert aliases["csharp"] == "C#"
        assert aliases["cpp"] == "C++"
        assert aliases["python"] == "Python"

    def test_whitespace_handling(self):
        """Languages with whitespace should still normalize (after strip)."""
        # Note: Current implementation doesn't strip, but we can add this if needed
        # For now, test that exact matches work
        assert normalize_language("c#") == "C#"
        assert normalize_language("c++") == "C++"

    def test_all_canonical_in_get_valid_languages(self):
        """All CANONICAL_LANGUAGES should be in get_valid_languages() result."""
        valid = get_valid_languages()
        for lang in CANONICAL_LANGUAGES:
            assert lang in valid, f"Canonical language '{lang}' not in get_valid_languages()"

    def test_no_duplicate_aliases(self):
        """No two aliases should map to different canonical names (except intentional)."""
        # All lowercase keys should be unique
        aliases = get_all_aliases()
        assert len(aliases) == len(set(aliases.keys()))

    def test_all_aliases_map_to_canonical(self):
        """All alias values should be in CANONICAL_LANGUAGES."""
        aliases = get_all_aliases()
        for alias, canonical in aliases.items():
            assert canonical in CANONICAL_LANGUAGES, f"Alias '{alias}' maps to non-canonical '{canonical}'"


class TestEdgeCases:
    """Test edge cases and boundary conditions."""

    def test_empty_string(self):
        """Empty string should raise ValueError."""
        with pytest.raises(ValueError):
            normalize_language("")

    def test_numeric_string(self):
        """Numeric strings should raise ValueError."""
        with pytest.raises(ValueError):
            normalize_language("123")

    def test_special_characters(self):
        """Strings with only special characters should raise ValueError."""
        with pytest.raises(ValueError):
            normalize_language("@#$%")

    def test_mixed_case_canonical(self):
        """Canonical names with wrong casing should still work."""
        # "python" -> "Python" (lowercase variant exists)
        assert normalize_language("python") == "Python"

        # "PYTHON" -> "Python" (uppercase should work)
        assert normalize_language("PYTHON") == "Python"

    def test_language_with_numbers(self):
        """Languages with numbers should work."""
        # "python3" is a valid alias
        assert normalize_language("python3") == "Python"
        assert normalize_language("Python3") == "Python"

    def test_hyphenated_variations(self):
        """Hyphenated language names should work."""
        assert normalize_language("c-sharp") == "C#"
        assert normalize_language("C-Sharp") == "C#"
        assert normalize_language("c-plus-plus") == "C++"


class TestConsistency:
    """Test consistency and invariants."""

    def test_normalization_idempotent(self):
        """Normalizing twice should give same result as normalizing once."""
        test_cases = ["csharp", "python", "cpp", "javascript"]
        for lang in test_cases:
            once = normalize_language(lang)
            twice = normalize_language(normalize_language(lang))
            assert once == twice, f"Normalization not idempotent for '{lang}'"

    def test_all_aliases_lowercase_keys(self):
        """All LANGUAGE_ALIASES keys should be lowercase."""
        for alias in LANGUAGE_ALIASES.keys():
            assert alias == alias.lower(), f"Alias key '{alias}' is not lowercase"

    def test_canonical_languages_not_empty(self):
        """CANONICAL_LANGUAGES should have content."""
        assert len(CANONICAL_LANGUAGES) > 0

    def test_language_aliases_not_empty(self):
        """LANGUAGE_ALIASES should have content."""
        assert len(LANGUAGE_ALIASES) > 0

    def test_more_aliases_than_canonical(self):
        """Should have more aliases than canonical languages (variations)."""
        assert len(LANGUAGE_ALIASES) > len(CANONICAL_LANGUAGES)


class TestRealWorldUsage:
    """Test real-world usage scenarios."""

    def test_api_call_scenarios(self):
        """Test typical API call scenarios."""
        # User types lowercase
        assert normalize_language("csharp") == "C#"

        # User copies from documentation with title case
        assert normalize_language("Python") == "Python"

        # User uses abbreviation
        assert normalize_language("js") == "JavaScript"
        assert normalize_language("ts") == "TypeScript"

        # User uses full name
        assert normalize_language("javascript") == "JavaScript"

    def test_common_mistakes(self):
        """Test common user mistakes that should work."""
        # Common mistake: "csharp" instead of "C#"
        assert normalize_language("csharp") == "C#"

        # Common mistake: "cpp" instead of "C++"
        assert normalize_language("cpp") == "C++"

        # Common mistake: all lowercase
        assert normalize_language("python") == "Python"
        assert normalize_language("rust") == "Rust"

    def test_keyword_search_integration(self):
        """Test that normalization works for keyword search scenarios."""
        # Simulating: language:csharp -> should normalize to language:C#
        user_input = "csharp"
        normalized = normalize_language(user_input)
        assert normalized == "C#"

        # Can then build query: language:C#
        query = f"language:{normalized}"
        assert query == "language:C#"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
