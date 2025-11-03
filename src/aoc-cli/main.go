package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/spf13/pflag"

	"aoc/builder"
	"aoc/config"
	"aoc/downloader"
	"aoc/runner"
)

func main() {
	var (
		year       = pflag.IntP("year", "y", 0, "Year to run (e.g., 2016)")
		day        = pflag.IntP("day", "d", 0, "Day to run (1-25)")
		all        = pflag.BoolP("all", "a", false, "Run all available solutions")
		resultsFile = pflag.StringP("results", "r", "results.yaml", "Path to results.yaml file")
	)

	pflag.Parse()

	// Get root directory (assume we're running from the root)
	rootDir, err := os.Getwd()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to get working directory: %v\n", err)
		os.Exit(1)
	}

	// If results file is relative, make it absolute from rootDir
	if !filepath.IsAbs(*resultsFile) {
		*resultsFile = filepath.Join(rootDir, *resultsFile)
	}

	// Load results configuration
	results, err := config.LoadResults(*resultsFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Warning: Failed to load results file: %v\n", err)
		fmt.Fprintf(os.Stderr, "Continuing without expected results...\n\n")
		results = &config.Results{Years: make(map[int]map[int]config.DayConfig)}
	}

	// Create services
	dl := downloader.New(rootDir)
	b := builder.New(rootDir)
	r := runner.New(rootDir)

	// Determine what to run
	if *all {
		runAll(results, dl, b, r)
	} else if *year != 0 && *day != 0 {
		runDay(*year, *day, results, dl, b, r)
	} else if *year != 0 {
		runYear(*year, results, dl, b, r)
	} else {
		pflag.Usage()
		os.Exit(1)
	}
}

func runDay(year, day int, results *config.Results, dl *downloader.Downloader, b *builder.Builder, r *runner.Runner) {
	fmt.Printf("=== Running AoC %d Day %d ===\n\n", year, day)

	// Check if skipped
	if results.ShouldSkip(year, day) {
		fmt.Printf("⏭️  Skipped (marked as skip in results.yaml)\n")
		return
	}

	// Find solution
	solutionPath, err := b.FindSolution(year, day)
	if err != nil {
		fmt.Printf("⏭️  Skipped (no solution found)\n")
		return
	}
	fmt.Printf("Found solution: %s\n", solutionPath)

	// Get input
	fmt.Printf("Getting input...\n")
	input, err := dl.ReadInput(year, day)
	if err != nil {
		fmt.Fprintf(os.Stderr, "❌ Failed to get input: %v\n", err)
		os.Exit(1)
	}

	// Build solution
	paths, err := b.Build(year, day)
	if err != nil {
		fmt.Fprintf(os.Stderr, "❌ Failed to build: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("Built successfully\n\n")

	// Run solution
	result := r.RunDay(year, day, paths, input)

	// Display results
	printDayResult(result, results)

	if !result.Success {
		os.Exit(1)
	}
}

func runYear(year int, results *config.Results, dl *downloader.Downloader, b *builder.Builder, r *runner.Runner) {
	fmt.Printf("=== Running all days for AoC %d ===\n\n", year)

	yearData, ok := results.Years[year]
	if !ok {
		fmt.Printf("No configuration found for year %d\n", year)
		return
	}

	totalDays := 0
	passedDays := 0

	for day := 1; day <= 25; day++ {
		// Check if skipped
		if results.ShouldSkip(year, day) {
			continue
		}

		// Check if day exists in config or has a solution
		_, hasConfig := yearData[day]
		_, err := b.FindSolution(year, day)
		hasSolution := err == nil

		if !hasConfig && !hasSolution {
			continue
		}

		fmt.Printf("--- Day %d ---\n", day)
		totalDays++

		// Get input
		input, err := dl.ReadInput(year, day)
		if err != nil {
			fmt.Printf("❌ Failed to get input: %v\n\n", err)
			continue
		}

		// Build solution
		paths, err := b.Build(year, day)
		if err != nil {
			fmt.Printf("❌ Failed to build: %v\n\n", err)
			continue
		}

		// Run solution
		result := r.RunDay(year, day, paths, input)
		printDayResult(result, results)

		if result.Success {
			passedDays++
		}
		fmt.Println()
	}

	fmt.Printf("=== Summary: %d/%d days passed ===\n", passedDays, totalDays)

	if passedDays < totalDays {
		os.Exit(1)
	}
}

func runAll(results *config.Results, dl *downloader.Downloader, b *builder.Builder, r *runner.Runner) {
	fmt.Printf("=== Running all available solutions ===\n\n")

	totalTests := 0
	passedTests := 0

	for year := range results.Years {
		fmt.Printf("=== Year %d ===\n\n", year)

		for day := 1; day <= 25; day++ {
			// Check if skipped
			if results.ShouldSkip(year, day) {
				continue
			}

			// Check if solution exists
			_, err := b.FindSolution(year, day)
			if err != nil {
				continue
			}

			fmt.Printf("--- Day %d ---\n", day)
			totalTests++

			// Get input
			input, err := dl.ReadInput(year, day)
			if err != nil {
				fmt.Printf("❌ Failed to get input: %v\n\n", err)
				continue
			}

			// Build solution
			paths, err := b.Build(year, day)
			if err != nil {
				fmt.Printf("❌ Failed to build: %v\n\n", err)
				continue
			}

			// Run solution
			result := r.RunDay(year, day, paths, input)
			printDayResult(result, results)

			if result.Success {
				passedTests++
			}
			fmt.Println()
		}
	}

	fmt.Printf("=== Summary: %d/%d tests passed ===\n", passedTests, totalTests)

	if passedTests < totalTests {
		os.Exit(1)
	}
}

func printDayResult(result runner.DayResult, results *config.Results) {
	// Part 1
	fmt.Printf("Part 1: %s", result.Part1.Output)
	if result.Part1.Error != nil {
		fmt.Printf(" ❌ Error: %v\n", result.Part1.Error)
	} else {
		fmt.Printf(" [%v]", result.Part1.Duration)

		if expected, ok := results.GetExpectedResult(result.Year, result.Day, 1); ok {
			if result.Part1.Output == expected {
				fmt.Printf(" ✅")
			} else {
				fmt.Printf(" ❌ (expected: %s)", expected)
			}
		}
		fmt.Println()
	}

	// Part 2
	fmt.Printf("Part 2: %s", result.Part2.Output)
	if result.Part2.Error != nil {
		fmt.Printf(" ❌ Error: %v\n", result.Part2.Error)
	} else {
		fmt.Printf(" [%v]", result.Part2.Duration)

		if expected, ok := results.GetExpectedResult(result.Year, result.Day, 2); ok {
			if result.Part2.Output == expected {
				fmt.Printf(" ✅")
			} else {
				fmt.Printf(" ❌ (expected: %s)", expected)
			}
		}
		fmt.Println()
	}

	// Total time
	totalDuration := result.Part1.Duration + result.Part2.Duration
	fmt.Printf("Total: %v\n", totalDuration)
}

func init() {
	pflag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: %s [OPTIONS]\n\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "Run Advent of Code solutions and verify results.\n\n")
		fmt.Fprintf(os.Stderr, "Options:\n")
		pflag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "\nExamples:\n")
		fmt.Fprintf(os.Stderr, "  %s --year 2016 --day 1    # Run a specific day (long form)\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "  %s -y 2016 -d 1           # Run a specific day (short form)\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "  %s --year 2016            # Run all days in 2016\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "  %s --all                  # Run all available solutions\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "  %s -a                     # Run all (short form)\n", filepath.Base(os.Args[0]))
	}
}
