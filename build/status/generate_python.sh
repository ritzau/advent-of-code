#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "$0")/read_workspace_status.sh"
read_workspace_status

output_file="$1"

cat >"${output_file}" <<'EOF'
"""Workspace status variables."""

EOF

for key in "${!WORKSPACE_DEFAULTS[@]}"; do
	val="${WORKSPACE_VALUES[${key}]:-${WORKSPACE_DEFAULTS[${key}]}}"

	# Unescape \n for body
	if [[ "${key}" == "STABLE_GIT_COMMIT_BODY" ]]; then
		val="${val//\\n/$'\n'}"
	fi

	var_name=$(echo "${key}" | sed 's/^STABLE_//' | tr '[:upper:]' '[:lower:]')

	# Escape for Python triple-quoted strings
	val="${val//\\/\\\\}"
	val="${val//\"/\\\"}"

	echo "${var_name^^} = \"\"\"${val}\"\"\"" >>"${output_file}"
done
