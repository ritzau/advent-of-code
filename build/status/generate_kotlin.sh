#!/usr/bin/env bash
set -euo pipefail

source "$(dirname "$0")/read_workspace_status.sh"
read_workspace_status

output_file="$1"

cat >"${output_file}" <<'EOF'
package workspace

object WorkspaceStatus {
EOF

for key in "${!WORKSPACE_DEFAULTS[@]}"; do
	val="${WORKSPACE_VALUES[${key}]:-${WORKSPACE_DEFAULTS[${key}]}}"

	# Unescape \n for body
	if [[ "${key}" == "STABLE_GIT_COMMIT_BODY" ]]; then
		val="${val//\\n/$'\n'}"
	fi

	# Convert STABLE_GIT_COMMIT_SHA -> gitCommitSha
	# Remove STABLE_ prefix, convert to camelCase
	var_name=$(echo "${key}" | sed 's/^STABLE_//')
	# Split on underscore and convert to camelCase
	var_name=$(echo "${var_name}" | awk -F_ '{
        printf "%s", tolower($1);
        for(i=2; i<=NF; i++) {
            printf "%s%s", toupper(substr($i,1,1)), tolower(substr($i,2))
        }
    }')

	# Escape for Kotlin triple-quoted strings
	val="${val//\$/\\\$}"

	echo "    const val ${var_name} = \"\"\"${val}\"\"\"" >>"${output_file}"
done

cat >>"${output_file}" <<'EOF'
}
EOF
