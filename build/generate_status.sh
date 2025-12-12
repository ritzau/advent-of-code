#!/usr/bin/env bash
set -euo pipefail

function dirty_state() {
	if [ -z "$(git status --porcelain)" ]; then
		echo "clean"
	else
		echo "dirty"
	fi
}

# Get tags for current commit (comma-separated if multiple)
function get_tags() {
	local tags=$(git tag --points-at HEAD | tr '\n' ',')
	# Remove trailing comma
	echo "${tags%,}"
}

echo "STABLE_GIT_COMMIT_SHA $(git rev-parse HEAD)"
echo "STABLE_GIT_BRANCH $(git rev-parse --abbrev-ref HEAD)"
if date --version >/dev/null 2>&1; then
	# GNU date (Linux)
	echo "STABLE_GIT_COMMIT_TIME $(date -u -d "@$(git log -1 --pretty=%ct)" +"%Y-%m-%dT%H:%M:%SZ")"
else
	# BSD date (macOS)
	echo "STABLE_GIT_COMMIT_TIME $(date -u -r $(git log -1 --pretty=%ct) +"%Y-%m-%dT%H:%M:%SZ")"
fi
echo "STABLE_GIT_COMMIT_TITLE $(git log -1 --pretty=format:'%s')"
echo "STABLE_GIT_COMMIT_BODY $(git log -1 --pretty=format:'%b' | tr '\n' '\\n')"
echo "STABLE_GIT_TAGS $(get_tags)"
echo "STABLE_GIT_IS_DIRTY $(dirty_state)"
