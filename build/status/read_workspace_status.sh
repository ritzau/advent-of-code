#!/usr/bin/env bash
# Source this file to get workspace status variables

declare -gA WORKSPACE_DEFAULTS=(
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

declare -gA WORKSPACE_VALUES

function read_workspace_status() {
	# Read stable status
	if [ -f bazel-out/stable-status.txt ]; then
		while IFS=' ' read -r key value || [ -n "${key:-}" ]; do
			[ -n "${key}" ] && WORKSPACE_VALUES["${key}"]="${value}"
		done <bazel-out/stable-status.txt
	fi

	# Read volatile status
	if [ -f bazel-out/volatile-status.txt ]; then
		while IFS=' ' read -r key value || [ -n "${key:-}" ]; do
			[ -n "${key}" ] && WORKSPACE_VALUES["${key}"]="${value}"
		done <bazel-out/volatile-status.txt
	fi
}
