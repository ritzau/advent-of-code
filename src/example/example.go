package main

import (
	"fmt"
	"foo/workspace"
)

func main() {
	fmt.Println("SHA:", workspace.GitCommitSha)
	fmt.Println("Branch:", workspace.GitBranch)
	fmt.Println("Body:", workspace.GitCommitBody)
}
