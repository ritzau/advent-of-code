#!/usr/bin/env bash
# Lint Kotlin sources with ktlint

set -euo pipefail

echo "Linting Kotlin sources..."
ktlint "src/**/*.kt"
echo "Lint complete!"
