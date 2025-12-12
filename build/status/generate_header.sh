#!/usr/bin/env bash
set -euo pipefail
output_file="$1"
cat >"$output_file" <<'EOF'
#pragma once
#include <string_view>
namespace workspace {
EOF
# Define defaults (both stable and volatile)
declare -A defaults=(
	[BUILD_EMBED_LABEL]=""
	[BUILD_HOST]="unknown"
	[BUILD_USER]="unknown"
	[BUILD_TIMESTAMP]="0"
	[FORMATTED_DATE]="unknown"
	[STABLE_GIT_COMMIT_SHA]="unknown"
	[STABLE_GIT_BRANCH]="unknown"
	[STABLE_GIT_IS_DIRTY]="unknown"
	[STABLE_GIT_COMMIT_TITLE]=""
	[STABLE_GIT_COMMIT_BODY]=""
	[STABLE_GIT_COMMIT_TIME]="1970-01-01T00:00:00Z"
	[STABLE_GIT_TAGS]=""
)
# Read actual values from both files
declare -A values
# Read stable status
if [ -f bazel-out/stable-status.txt ]; then
	while IFS=' ' read -r key value || [ -n "${key:-}" ]; do
		[ -n "${key}" ] && values["${key}"]="${value}"
	done <bazel-out/stable-status.txt
fi
# Read volatile status
if [ -f bazel-out/volatile-status.txt ]; then
	while IFS=' ' read -r key value || [ -n "${key:-}" ]; do
		[ -n "${key}" ] && values["${key}"]="${value}"
	done <bazel-out/volatile-status.txt
fi
# Generate with defaults fallback
for key in "${!defaults[@]}"; do
	val="${values[${key}]:-${defaults[${key}]}}"

	# Unescape \n to real newlines only for body
	if [[ "${key}" == "STABLE_GIT_COMMIT_BODY" ]]; then
		val="${val//\\n/$'\n'}"
	fi

	# Convert key to lowercase and remove STABLE_ prefix
	var_name=$(echo "${key}" | sed 's/^STABLE_//' | tr '[:upper:]' '[:lower:]')

	if echo "${val}" | grep -qF ')DELIM"'; then
		echo "Error: Value contains delimiter" >&2
		exit 1
	fi

	echo "inline constexpr std::string_view ${var_name} = R\"DELIM(${val})DELIM\";" >>"$output_file"
done
cat >>"$output_file" <<'EOF'
}  // namespace workspace
EOF
