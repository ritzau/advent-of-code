#!/usr/bin/env python3

from workspace_status import (
    GIT_COMMIT_SHA,
    GIT_BRANCH,
    GIT_COMMIT_BODY,
)

print(f"SHA: {GIT_COMMIT_SHA}")
print(f"Branch: {GIT_BRANCH}")
print(f"Body:\n{GIT_COMMIT_BODY}")
