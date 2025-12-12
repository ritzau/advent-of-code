#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "$0")/read_workspace_status.sh"
read_workspace_status

output_file="$1"

cat >"${output_file}" <<'EOF'
package workspace

EOF

for key in "${!WORKSPACE_DEFAULTS[@]}"; do
	val="${WORKSPACE_VALUES[${key}]:-${WORKSPACE_DEFAULTS[${key}]}}"

	# Unescape \n for body
	if [[ "${key}" == "STABLE_GIT_COMMIT_BODY" ]]; then
		val="${val//\\n/$'\n'}"
	fi

	# Convert STABLE_GIT_COMMIT_SHA -> GitCommitSha (PascalCase)
	# Remove STABLE_ prefix
	var_name=$(echo "${key}" | sed 's/^STABLE_//')
	# Convert to PascalCase (each word capitalized, no underscores)
	var_name=$(echo "${var_name}" | awk -F_ '{
        for(i=1; i<=NF; i++) {
            printf "%s%s", toupper(substr($i,1,1)), tolower(substr($i,2))
        }
    }')

	# Escape backticks and backslashes for Go
	val="${val//\\/\\\\}"
	val="${val//\`/\\\`}"

	echo "const ${var_name} = \`${val}\`" >>"${output_file}"
done
