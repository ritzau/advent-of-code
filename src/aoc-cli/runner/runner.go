package runner

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
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
func (r *Runner) RunPart(paths *builder.SolutionPaths, part int, input string) Result {
	start := time.Now()

	// Use binary path
	binaryPath := paths.Part1
	if part == 2 {
		binaryPath = paths.Part2
	}

	// Optimize Julia execution: use system Julia if available to avoid --compiled-modules=no overhead
	var cmd *exec.Cmd
	if paths.Language == "julia" && strings.HasSuffix(binaryPath, ".sh") {
		if juliaPath, err := exec.LookPath("julia"); err == nil {
			// Extract the Julia source file from the wrapper script
			// The wrapper sets MAIN="$(rlocation ...part1.jl)"
			// We can find the source file in the solution directory
			juliaScript := r.findJuliaScript(paths.SolutionDir, part)
			if juliaScript != "" {
				cmd = exec.Command(juliaPath, juliaScript)
			}
		}
	}

	// Fall back to bazel wrapper if Julia optimization didn't work
	if cmd == nil {
		cmd = exec.Command(binaryPath)
	}

	cmd.Dir = r.rootDir // Set working directory to repo root for relative path resolution

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

// findJuliaScript finds the Julia source file for a given part
func (r *Runner) findJuliaScript(solutionDir string, part int) string {
	partFile := fmt.Sprintf("part%d.jl", part)
	scriptPath := filepath.Join(solutionDir, "src", partFile)

	// Check if file exists
	if _, err := os.Stat(scriptPath); err == nil {
		return scriptPath
	}

	return ""
}

// RunDay runs both parts of a day's solution
func (r *Runner) RunDay(year, day int, paths *builder.SolutionPaths, input string) DayResult {
	result := DayResult{
		Year:    year,
		Day:     day,
		Success: true,
	}

	// Run part 1
	result.Part1 = r.RunPart(paths, 1, input)
	if result.Part1.Error != nil {
		result.Success = false
	}

	// Run part 2
	result.Part2 = r.RunPart(paths, 2, input)
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
