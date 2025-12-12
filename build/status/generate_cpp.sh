#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "$0")/read_workspace_status.sh"
read_workspace_status

output_file="$1"

cat >"${output_file}" <<'EOF'
#pragma once
#include <string_view>
namespace workspace {
EOF

for key in "${!WORKSPACE_DEFAULTS[@]}"; do
	val="${WORKSPACE_VALUES[${key}]:-${WORKSPACE_DEFAULTS[${key}]}}"

	# Unescape \n for body
	if [[ "${key}" == "STABLE_GIT_COMMIT_BODY" ]]; then
		val="${val//\\n/$'\n'}"
	fi

	var_name=$(echo "${key}" | sed 's/^STABLE_//' | tr '[:upper:]' '[:lower:]')

	if echo "${val}" | grep -qF ')DELIM"'; then
		echo "Error: Value contains delimiter" >&2
		exit 1
	fi

	echo "inline constexpr std::string_view ${var_name} = R\"DELIM(${val})DELIM\";" >>"${output_file}"
done

cat >>"${output_file}" <<'EOF'
}  // namespace workspace
EOF
