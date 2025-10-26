package main

import (
	"fmt"
	"io"
	"os"
)

func solve(input string) int {
	lines := parseInput(input)
	_ = lines
	// TODO: Implement solution
	return 0
}

func main() {
	data, _ := io.ReadAll(os.Stdin)
	result := solve(string(data))
	fmt.Println(result)
}
