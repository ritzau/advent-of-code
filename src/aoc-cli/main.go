package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/spf13/pflag"

	"aoc/builder"
	"aoc/config"
	"aoc/downloader"
	"aoc/runner"
)

func main() {
	// Define flags
	var (
		year        = pflag.IntP("year", "y", 0, "Year to run (e.g., 2016)")
		day         = pflag.IntP("day", "d", 0, "Day to run (1-25)")
		all         = pflag.BoolP("all", "a", false, "Run all available solutions")
		lang        = pflag.StringSliceP("lang", "l", nil, "Filter by language (can be specified multiple times)")
		resultsFile = pflag.StringP("results", "r", "results.yaml", "Path to results.yaml file")
	)

	pflag.Parse()

	// Get root directory
	// When running via 'bazel run', use BUILD_WORKSPACE_DIRECTORY
	// Otherwise, try to find the repository root
	rootDir := os.Getenv("BUILD_WORKSPACE_DIRECTORY")
	if rootDir == "" {
		var err error
		rootDir, err = findRepoRoot()
		if err != nil {
			fmt.Fprintf(os.Stderr, "Failed to find repository root: %v\n", err)
			os.Exit(1)
		}
	}

	// Check if a command was provided
	args := pflag.Args()
	command := "run" // default command for backwards compatibility
	if len(args) > 0 {
		command = args[0]
	}

	// Execute the appropriate command
	switch command {
	case "run":
		runCommand(rootDir, *year, *day, *all, *lang, *resultsFile)
	case "download":
		downloadCommand(rootDir, *year, *day)
	default:
		fmt.Fprintf(os.Stderr, "Unknown command: %s\n\n", command)
		pflag.Usage()
		os.Exit(1)
	}
}

// findRepoRoot finds the repository root by looking for characteristic files
// Searches upwards from the current directory
func findRepoRoot() (string, error) {
	cwd, err := os.Getwd()
	if err != nil {
		return "", err
	}

	dir := cwd
	for {
		// Check for characteristic files that indicate repo root
		// Check for WORKSPACE or WORKSPACE.bazel (Bazel workspace)
		if _, err := os.Stat(filepath.Join(dir, "WORKSPACE")); err == nil {
			return dir, nil
		}
		if _, err := os.Stat(filepath.Join(dir, "WORKSPACE.bazel")); err == nil {
			return dir, nil
		}
		// Check for .git directory
		if _, err := os.Stat(filepath.Join(dir, ".git")); err == nil {
			return dir, nil
		}

		// Move up one directory
		parent := filepath.Dir(dir)
		if parent == dir {
			// Reached root without finding repo markers
			break
		}
		dir = parent
	}

	return "", fmt.Errorf("could not find repository root (no WORKSPACE, WORKSPACE.bazel, or .git found)")
}

// detectSolutionFromPath tries to detect year, day, and language from a path
// Returns (year, day, language, ok) where ok is true if detection succeeded
// If day is 0, it means run all days for that year
func detectSolutionFromPath(rootDir, cwd string) (int, int, string, bool) {
	// Try to get relative path from rootDir
	relPath, err := filepath.Rel(rootDir, cwd)
	if err != nil {
		return 0, 0, "", false
	}

	// Check if we're inside the src directory
	if !strings.HasPrefix(relPath, "src") {
		return 0, 0, "", false
	}

	// Split the path into components
	parts := strings.Split(relPath, string(filepath.Separator))

	// We need at least src/AoCXX
	if len(parts) < 2 {
		return 0, 0, "", false
	}

	// Parse year from AoCXX
	yearDir := parts[1]
	if !strings.HasPrefix(yearDir, "AoC") || len(yearDir) < 5 {
		return 0, 0, "", false
	}
	yearStr := yearDir[3:]
	year := 0
	if _, err := fmt.Sscanf(yearStr, "%d", &year); err != nil {
		return 0, 0, "", false
	}
	// Convert YY to full year (assuming 20YY)
	if year < 100 {
		year += 2000
	}

	// If we're just in the year directory (src/AoCXX), return year with day=0
	if len(parts) == 2 {
		return year, 0, "", true
	}

	// Parse solution directory (sXXeXX or sXXeXX-lang)
	solutionDir := parts[2]

	// Parse day from sXXeXX
	if !strings.HasPrefix(solutionDir, "s") {
		return 0, 0, "", false
	}

	// Find the 'e' separator
	eIndex := strings.Index(solutionDir, "e")
	if eIndex < 2 {
		return 0, 0, "", false
	}

	// Parse day number
	dayStr := solutionDir[eIndex+1:]
	// Remove language suffix if present
	if hyphenIndex := strings.Index(dayStr, "-"); hyphenIndex > 0 {
		dayStr = dayStr[:hyphenIndex]
	}

	day := 0
	if _, err := fmt.Sscanf(dayStr, "%d", &day); err != nil {
		return 0, 0, "", false
	}

	// Extract language if present
	language := ""
	if hyphenIndex := strings.Index(solutionDir, "-"); hyphenIndex > 0 {
		language = solutionDir[hyphenIndex+1:]
	}

	return year, day, language, true
}

func runCommand(rootDir string, year, day int, all bool, langSlice []string, resultsFile string) {
	// Convert language filter to a map for quick lookup
	langFilter := make(map[string]bool)
	if langSlice != nil && len(langSlice) > 0 {
		for _, l := range langSlice {
			langFilter[l] = true
		}
	}

	// If results file is relative, make it absolute from rootDir
	if !filepath.IsAbs(resultsFile) {
		resultsFile = filepath.Join(rootDir, resultsFile)
	}

	// Load results configuration
	results, err := config.LoadResults(resultsFile)
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
	if all {
		runAll(results, dl, b, r, langFilter)
	} else if year != 0 && day != 0 {
		runDay(year, day, results, dl, b, r, langFilter)
	} else if year != 0 {
		runYear(year, results, dl, b, r, langFilter)
	} else {
		// Try to detect solution from current working directory
		// When running via 'bazel run', use BUILD_WORKING_DIRECTORY (where bazel was invoked)
		// Otherwise, use current working directory
		cwd := os.Getenv("BUILD_WORKING_DIRECTORY")
		if cwd == "" {
			var err error
			cwd, err = os.Getwd()
			if err != nil {
				pflag.Usage()
				os.Exit(1)
			}
		}

		detectedYear, detectedDay, detectedLang, ok := detectSolutionFromPath(rootDir, cwd)
		if ok {
			// If language was detected and no language filter was set, use the detected language
			if detectedLang != "" && len(langFilter) == 0 {
				langFilter[detectedLang] = true
			}
			// If day is 0, run the whole year
			if detectedDay == 0 {
				runYear(detectedYear, results, dl, b, r, langFilter)
			} else {
				runDay(detectedYear, detectedDay, results, dl, b, r, langFilter)
			}
			return
		}

		pflag.Usage()
		os.Exit(1)
	}
}

func downloadCommand(rootDir string, year, day int) {
	if year == 0 || day == 0 {
		fmt.Fprintf(os.Stderr, "Error: --year and --day are required for the download command\n\n")
		pflag.Usage()
		os.Exit(1)
	}

	dl := downloader.New(rootDir)

	// Get the input (downloads if needed, uses cache if available)
	input, err := dl.ReadInput(year, day)
	if err != nil {
		fmt.Fprintf(os.Stderr, "❌ Failed to get input: %v\n", err)
		os.Exit(1)
	}

	// Output the input to stdout
	fmt.Print(input)
}

// shouldRunLanguage checks if a language should be run based on filters and config
func shouldRunLanguage(language string, langFilter map[string]bool, results *config.Results, year, day int) bool {
	// Check if language is filtered out by CLI flag
	if len(langFilter) > 0 && !langFilter[language] {
		return false
	}

	// Check if language is skipped in config
	if results.ShouldSkipLanguage(year, day, language) {
		return false
	}

	return true
}

type languageResult struct {
	day      int
	language string
	result   runner.DayResult
	err      error
}

func runDay(year, day int, results *config.Results, dl *downloader.Downloader, b *builder.Builder, r *runner.Runner, langFilter map[string]bool) {
	fmt.Printf("=== AoC %d Day %d ===\n", year, day)

	// Check if skipped
	if results.ShouldSkip(year, day) {
		fmt.Printf("⏭️  Skipped (marked as skip in results.yaml)\n")
		return
	}

	// Find all solutions
	solutions, err := b.FindSolutions(year, day)
	if err != nil {
		fmt.Printf("⏭️  Skipped (no solutions found)\n")
		return
	}

	// Get input
	input, err := dl.ReadInput(year, day)
	if err != nil {
		fmt.Fprintf(os.Stderr, "❌ Failed to get input: %v\n", err)
		os.Exit(1)
	}

	allSuccess := true
	ranCount := 0

	// Print table header
	printTableHeader(true)

	// Run each language implementation
	for _, solutionPath := range solutions {
		language := b.ExtractLanguage(solutionPath)

		// Check if we should run this language
		if !shouldRunLanguage(language, langFilter, results, year, day) {
			continue
		}

		ranCount++

		// Build solution
		paths, err := b.BuildSolutionForDay(year, day, solutionPath)
		if err != nil {
			fmt.Fprintf(os.Stderr, "❌ Failed to build %s: %v\n", language, err)
			printTableRow(day, language, nil, nil, true)
			allSuccess = false
			continue
		}

		// Run solution
		result := r.RunDay(year, day, paths, input)

		printTableRow(day, language, &result, results, true)

		if !result.Success {
			allSuccess = false
		}
	}

	if ranCount == 0 {
		fmt.Printf("\n⏭️  No solutions to run (all filtered or skipped)\n")
		return
	}

	// Print table footer
	printTableFooter(true)

	if !allSuccess {
		os.Exit(1)
	}
}

func runYear(year int, results *config.Results, dl *downloader.Downloader, b *builder.Builder, r *runner.Runner, langFilter map[string]bool) {
	fmt.Printf("=== Running all days for AoC %d ===\n", year)

	yearData, ok := results.Years[year]
	if !ok {
		fmt.Printf("No configuration found for year %d\n", year)
		return
	}

	totalTests := 0
	passedTests := 0
	hasResults := false

	printTableHeader(false)

	for day := 1; day <= 25; day++ {
		// Check if skipped
		if results.ShouldSkip(year, day) {
			continue
		}

		// Check if day exists in config or has solutions
		_, hasConfig := yearData[day]
		solutions, err := b.FindSolutions(year, day)
		hasSolutions := err == nil && len(solutions) > 0

		if !hasConfig && !hasSolutions {
			continue
		}

		// Get input
		input, err := dl.ReadInput(year, day)
		if err != nil {
			fmt.Fprintf(os.Stderr, "❌ Failed to get input for day %d: %v\n", day, err)
			continue
		}

		// Run each language implementation
		for _, solutionPath := range solutions {
			language := b.ExtractLanguage(solutionPath)

			// Check if we should run this language
			if !shouldRunLanguage(language, langFilter, results, year, day) {
				continue
			}

			totalTests++
			hasResults = true

			// Build solution
			paths, err := b.BuildSolutionForDay(year, day, solutionPath)
			if err != nil {
				fmt.Fprintf(os.Stderr, "❌ Failed to build day %d %s: %v\n", day, language, err)
				printTableRow(day, language, nil, nil, false)
				continue
			}

			// Run solution
			result := r.RunDay(year, day, paths, input)
			printTableRow(day, language, &result, results, false)

			if result.Success {
				passedTests++
			}
		}
	}

	if hasResults {
		printTableFooter(false)
		fmt.Printf("\n=== Summary: %d/%d tests passed ===\n", passedTests, totalTests)
	} else {
		fmt.Printf("\nNo solutions found\n")
	}

	if passedTests < totalTests {
		os.Exit(1)
	}
}

func runAll(results *config.Results, dl *downloader.Downloader, b *builder.Builder, r *runner.Runner, langFilter map[string]bool) {
	fmt.Printf("=== Running all available solutions ===\n")

	totalTests := 0
	passedTests := 0
	hasResults := false

	printTableHeader(false)

	for year := range results.Years {
		for day := 1; day <= 25; day++ {
			// Check if skipped
			if results.ShouldSkip(year, day) {
				continue
			}

			// Check if solutions exist
			solutions, err := b.FindSolutions(year, day)
			if err != nil {
				continue
			}

			// Get input
			input, err := dl.ReadInput(year, day)
			if err != nil {
				fmt.Fprintf(os.Stderr, "❌ Failed to get input for %d day %d: %v\n", year, day, err)
				continue
			}

			// Run each language implementation
			for _, solutionPath := range solutions {
				language := b.ExtractLanguage(solutionPath)

				// Check if we should run this language
				if !shouldRunLanguage(language, langFilter, results, year, day) {
					continue
				}

				totalTests++
				hasResults = true

				// Build solution
				paths, err := b.BuildSolutionForDay(year, day, solutionPath)
				if err != nil {
					fmt.Fprintf(os.Stderr, "❌ Failed to build %d day %d %s: %v\n", year, day, language, err)
					printTableRow(day, language, nil, nil, false)
					continue
				}

				// Run solution
				result := r.RunDay(year, day, paths, input)
				printTableRow(day, language, &result, results, false)

				if result.Success {
					passedTests++
				}
			}
		}
	}

	if hasResults {
		printTableFooter(false)
		fmt.Printf("\n=== Summary: %d/%d tests passed ===\n", passedTests, totalTests)
	} else {
		fmt.Printf("\nNo solutions found\n")
	}

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

func printDayResultWithLanguage(result runner.DayResult, results *config.Results, language string) {
	// Part 1
	fmt.Printf("Part 1: %s", result.Part1.Output)
	if result.Part1.Error != nil {
		fmt.Printf(" ❌ Error: %v\n", result.Part1.Error)
	} else {
		fmt.Printf(" [%v]", result.Part1.Duration)

		if expected, ok := results.GetExpectedResultForLanguage(result.Year, result.Day, 1, language); ok {
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

		if expected, ok := results.GetExpectedResultForLanguage(result.Year, result.Day, 2, language); ok {
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

const langWidth = 10 // Default width for language column

func printTableHeader(singleDay bool) {
	if singleDay {
		fmt.Printf("\n┌─%s─┬────────────────────────────┬────────────────────────────┬──────────────┐\n", repeatStr("─", langWidth))
		fmt.Printf("│ %-*s │ Part 1                     │ Part 2                     │ Total        │\n", langWidth, "Language")
		fmt.Printf("├─%s─┼────────────────────────────┼────────────────────────────┼──────────────┤\n", repeatStr("─", langWidth))
	} else {
		fmt.Printf("\n┌─────┬─%s─┬────────────────────────────┬────────────────────────────┬──────────────┐\n", repeatStr("─", langWidth))
		fmt.Printf("│ Day │ %-*s │ Part 1                     │ Part 2                     │ Total        │\n", langWidth, "Lang")
		fmt.Printf("├─────┼─%s─┼────────────────────────────┼────────────────────────────┼──────────────┤\n", repeatStr("─", langWidth))
	}
}

func printTableRow(day int, language string, result *runner.DayResult, results *config.Results, singleDay bool) {
	if result == nil {
		// Build error
		if singleDay {
			fmt.Printf("│ %-*s │ Build Error                │                            │              │\n", langWidth, language)
		} else {
			fmt.Printf("│ %-3d │ %-*s │ Build Error                │                            │              │\n", day, langWidth, language)
		}
		return
	}

	// Format Part 1
	expected1, hasExpected1 := results.GetExpectedResultForLanguage(result.Year, result.Day, 1, language)
	part1Str := formatPartResult(result.Part1, expected1, hasExpected1)

	// Format Part 2
	expected2, hasExpected2 := results.GetExpectedResultForLanguage(result.Year, result.Day, 2, language)
	part2Str := formatPartResult(result.Part2, expected2, hasExpected2)

	// Format Total
	totalDuration := result.Part1.Duration + result.Part2.Duration
	totalStr := formatDuration(totalDuration)

	if singleDay {
		fmt.Printf("│ %-*s │ %s │ %s │ %-12s │\n", langWidth, language, part1Str, part2Str, totalStr)
	} else {
		fmt.Printf("│ %-3d │ %-*s │ %s │ %s │ %-12s │\n", day, langWidth, language, part1Str, part2Str, totalStr)
	}
}

func printTableFooter(singleDay bool) {
	if singleDay {
		fmt.Printf("└─%s─┴────────────────────────────┴────────────────────────────┴──────────────┘\n", repeatStr("─", langWidth))
	} else {
		fmt.Printf("└─────┴─%s─┴────────────────────────────┴────────────────────────────┴──────────────┘\n", repeatStr("─", langWidth))
	}
}

func formatPartResult(r runner.Result, expectedResult string, hasExpected bool) string {
	if r.Error != nil {
		return "    Error                   "
	}

	output := r.Output
	if len(output) > 24 {
		output = output[:24] + "…"
	}

	// Checkmarks take 2 visual columns, so we pad to 24 chars + space + checkmark (2 visual) = 27 visual columns
	// We need to add one more space after checkmark to reach 28 total
	// Without checkmark, we pad to 28 chars
	if hasExpected {
		if r.Output == expectedResult {
			return fmt.Sprintf("%-22s ✅ ", output)
		}
		return fmt.Sprintf("%-22s ❌ ", output)
	}

	return fmt.Sprintf("%-28s", output)
}

func formatDuration(d time.Duration) string {
	if d < time.Millisecond {
		return fmt.Sprintf("%.2fµs", float64(d.Microseconds()))
	} else if d < time.Second {
		return fmt.Sprintf("%.2fms", float64(d.Microseconds())/1000.0)
	}
	return fmt.Sprintf("%.2fs", d.Seconds())
}

func repeatStr(s string, n int) string {
	result := ""
	for i := 0; i < n; i++ {
		result += s
	}
	return result
}

func init() {
	pflag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage: %s [COMMAND] [OPTIONS]\n\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "Run Advent of Code solutions and verify results.\n\n")
		fmt.Fprintf(os.Stderr, "Commands:\n")
		fmt.Fprintf(os.Stderr, "  run       Run solutions (default command)\n")
		fmt.Fprintf(os.Stderr, "  download  Download puzzle input to stdout\n\n")
		fmt.Fprintf(os.Stderr, "Options:\n")
		pflag.PrintDefaults()
		fmt.Fprintf(os.Stderr, "\nExamples:\n")
		fmt.Fprintf(os.Stderr, "  %s run --year 2016 --day 1        # Run a specific day\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "  %s -y 2016 -d 1                   # Run a specific day (default command)\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "  %s run -y 2016 -d 1 -l go         # Run only Go implementation\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "  %s run -y 2016 -d 1 -l go -l rust # Run Go and Rust implementations\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "  %s run --year 2016                # Run all days in 2016\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "  %s run --all                      # Run all available solutions\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "  %s run -a -l default              # Run all default implementations\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "\n  # Auto-detect from directory:\n")
		fmt.Fprintf(os.Stderr, "  cd src/AoC16/s16e01-go && %s      # Run day 1 (go only)\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "  cd src/AoC16 && %s                # Run all days in 2016\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "\n")
		fmt.Fprintf(os.Stderr, "  %s download -y 2016 -d 1          # Download input to stdout\n", filepath.Base(os.Args[0]))
		fmt.Fprintf(os.Stderr, "  %s download -y 2016 -d 1 | wc -l  # Pipe input to other commands\n", filepath.Base(os.Args[0]))
	}
}
