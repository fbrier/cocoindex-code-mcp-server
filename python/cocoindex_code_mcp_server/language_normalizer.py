#!/usr/bin/env python3
#
# SPDX-License-Identifier: AGPL-3.0-or-later
# SPDX-FileCopyrightText: 2025 aanno <aanno@users.noreply.github.com>
#
# This file is part of cocoindex_code_mcp_server from
# https://github.com/aanno/cocoindex-code-mcp-server

"""
Language name normalization for MCP API.

Provides case-insensitive mapping from common language name variations
to canonical database values.
"""

from typing import Optional

# Canonical language names as stored in database
CANONICAL_LANGUAGES = {
    "C#",
    "C++",
    "C",
    "Python",
    "JavaScript",
    "TypeScript",
    "Java",
    "Rust",
    "Go",
    "Kotlin",
    "Swift",
    "Ruby",
    "PHP",
    "Haskell",
    "JSON",
    "Markdown",
    "ps1",  # PowerShell
    "sh",   # Shell script
    "sln",  # Visual Studio solution
}

# Map common variations to canonical names (all lowercase keys)
LANGUAGE_ALIASES = {
    # C# variations
    "c#": "C#",
    "csharp": "C#",
    "cs": "C#",
    "c-sharp": "C#",

    # C++ variations
    "c++": "C++",
    "cpp": "C++",
    "cplusplus": "C++",
    "c-plus-plus": "C++",

    # C variations
    "c": "C",

    # Python variations
    "python": "Python",
    "py": "Python",
    "python3": "Python",

    # JavaScript variations
    "javascript": "JavaScript",
    "js": "JavaScript",
    "ecmascript": "JavaScript",

    # TypeScript variations
    "typescript": "TypeScript",
    "ts": "TypeScript",

    # Java variations
    "java": "Java",

    # Rust variations
    "rust": "Rust",
    "rs": "Rust",

    # Go variations
    "go": "Go",
    "golang": "Go",

    # Kotlin variations
    "kotlin": "Kotlin",
    "kt": "Kotlin",

    # Swift variations
    "swift": "Swift",

    # Ruby variations
    "ruby": "Ruby",
    "rb": "Ruby",

    # PHP variations
    "php": "PHP",

    # Haskell variations
    "haskell": "Haskell",
    "hs": "Haskell",

    # JSON variations
    "json": "JSON",

    # Markdown variations
    "markdown": "Markdown",
    "md": "Markdown",

    # PowerShell variations
    "ps1": "ps1",
    "powershell": "ps1",
    "pwsh": "ps1",

    # Shell script variations
    "sh": "sh",
    "shell": "sh",
    "bash": "sh",

    # Visual Studio solution
    "sln": "sln",
    "solution": "sln",
}


def normalize_language(language: Optional[str]) -> Optional[str]:
    """
    Normalize language name to canonical form using case-insensitive lookup.

    Args:
        language: Language name in any common variation (e.g., "csharp", "C#", "python")

    Returns:
        Canonical language name (e.g., "C#", "Python") or None if input is None

    Raises:
        ValueError: If language is not recognized

    Examples:
        >>> normalize_language("csharp")
        'C#'
        >>> normalize_language("C#")
        'C#'
        >>> normalize_language("cpp")
        'C++'
        >>> normalize_language("python")
        'Python'
        >>> normalize_language("invalid")
        Traceback: ValueError: Invalid language 'invalid'...
    """
    if language is None:
        return None

    # Quick path: exact match with canonical name (case-sensitive)
    if language in CANONICAL_LANGUAGES:
        return language

    # Case-insensitive lookup in aliases
    language_lower = language.lower()
    if language_lower in LANGUAGE_ALIASES:
        return LANGUAGE_ALIASES[language_lower]

    # Not found - generate helpful error message
    raise ValueError(
        f"Invalid language '{language}'. "
        f"Valid languages: {', '.join(sorted(CANONICAL_LANGUAGES))}\n\n"
        f"Common variations are also accepted (e.g., 'csharp' -> 'C#', 'cpp' -> 'C++', 'py' -> 'Python')"
    )


def get_valid_languages() -> list[str]:
    """Get list of canonical language names."""
    return sorted(CANONICAL_LANGUAGES)


def get_all_aliases() -> dict[str, str]:
    """Get mapping of all aliases to canonical names (for documentation)."""
    return dict(LANGUAGE_ALIASES)


# Examples for documentation/testing
NORMALIZATION_EXAMPLES = {
    "C# variations": {
        "csharp": "C#",
        "CSharp": "C#",
        "c#": "C#",
        "cs": "C#",
    },
    "C++ variations": {
        "cpp": "C++",
        "cplusplus": "C++",
        "c++": "C++",
    },
    "Python variations": {
        "python": "Python",
        "py": "Python",
        "python3": "Python",
    },
    "Case-insensitive": {
        "PYTHON": "Python",
        "javascript": "JavaScript",
        "RUST": "Rust",
    },
}


if __name__ == "__main__":
    # Self-test
    print("Language Normalization Examples:")
    print("=" * 60)

    for category, examples in NORMALIZATION_EXAMPLES.items():
        print(f"\n{category}:")
        for input_lang, expected in examples.items():
            try:
                result = normalize_language(input_lang)
                status = "OK" if result == expected else "FAIL"
                print(f"  [{status}] {input_lang:15} -> {result}")
            except ValueError as e:
                print(f"  [FAIL] {input_lang:15} -> ERROR: {e}")

    print("\n" + "=" * 60)
    print("\nValid canonical languages:")
    for lang in get_valid_languages():
        print(f"  - {lang}")

    print("\n" + "=" * 60)
    print("\nTesting invalid language:")
    try:
        normalize_language("cobol")
    except ValueError as e:
        print(f"  Error (expected): {e}")
