package runner

import (
	"bytes"
	"fmt"
	"os/exec"
	"strings"
	"time"

	"aoc/builder"
)

// Result represents the result of running a solution part
type Result struct {
	Part     int
	Output   string
	Duration time.Duration
	Error    error
}

// DayResult represents the results of running both parts of a day
type DayResult struct {
	Year    int
	Day     int
	Part1   Result
	Part2   Result
	Success bool
}

// Runner handles running AoC solutions
type Runner struct {
	rootDir string
}

// New creates a new Runner
func New(rootDir string) *Runner {
	return &Runner{rootDir: rootDir}
}

// RunPart runs a single part of a solution
func (r *Runner) RunPart(binaryPath string, input string, part int) Result {
	start := time.Now()

	cmd := exec.Command(binaryPath)
	cmd.Stdin = strings.NewReader(input)

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err := cmd.Run()
	duration := time.Since(start)

	if err != nil {
		return Result{
			Part:     part,
			Output:   "",
			Duration: duration,
			Error:    fmt.Errorf("execution failed: %w (stderr: %s)", err, stderr.String()),
		}
	}

	// Trim whitespace from output
	output := strings.TrimSpace(stdout.String())

	return Result{
		Part:     part,
		Output:   output,
		Duration: duration,
		Error:    nil,
	}
}

// RunDay runs both parts of a day's solution
func (r *Runner) RunDay(year, day int, paths *builder.SolutionPaths, input string) DayResult {
	result := DayResult{
		Year:    year,
		Day:     day,
		Success: true,
	}

	// Run part 1
	result.Part1 = r.RunPart(paths.Part1, input, 1)
	if result.Part1.Error != nil {
		result.Success = false
	}

	// Run part 2
	result.Part2 = r.RunPart(paths.Part2, input, 2)
	if result.Part2.Error != nil {
		result.Success = false
	}

	return result
}

// VerifyResult checks if the result matches the expected value
func VerifyResult(result Result, expected string) (bool, string) {
	if result.Error != nil {
		return false, fmt.Sprintf("Error: %v", result.Error)
	}

	if result.Output == expected {
		return true, "✅ Pass"
	}

	return false, fmt.Sprintf("❌ Fail (got: %s, expected: %s)", result.Output, expected)
}
