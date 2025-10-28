package main

import (
	"fmt"
	"io"
	"os"
	"time"

	"s16e01/common"
)

func main() {
	data, err := io.ReadAll(os.Stdin)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to read from stdin: %v\n", err)
		os.Exit(1)
	}

	input := string(data)

	fmt.Println("AoC 2016 Day 1: No Time for a Taxicab")
	fmt.Println("======================================")

	// Part 1
	start := time.Now()
	result1 := common.SolvePart1(input)
	duration1 := time.Since(start)
	const expectedPart1 = 300
	pass1 := result1 == expectedPart1

	emoji1 := "✅"
	if !pass1 {
		emoji1 = "❌"
	}

	fmt.Printf("Part 1: %s %d (expected: %d) [%v]\n",
		emoji1, result1, expectedPart1, duration1)

	// Part 2
	start = time.Now()
	result2 := common.SolvePart2(input)
	duration2 := time.Since(start)
	const expectedPart2 = 159
	pass2 := result2 == expectedPart2

	emoji2 := "✅"
	if !pass2 {
		emoji2 = "❌"
	}

	fmt.Printf("Part 2: %s %d (expected: %d) [%v]\n",
		emoji2, result2, expectedPart2, duration2)

	fmt.Printf("Total: %v\n", duration1+duration2)

	if pass1 && pass2 {
		fmt.Println("\n🌟🌟 All tests passed!")
	} else {
		fmt.Println("\n❌ Some tests failed")
		os.Exit(1)
	}
}
