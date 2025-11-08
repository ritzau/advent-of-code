package builder

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// Builder handles building AoC solutions
type Builder struct {
	rootDir    string
	builtFlakes map[string]bool // Track which flakes have been built in this session
}

// New creates a new Builder
func New(rootDir string) *Builder {
	return &Builder{
		rootDir:     rootDir,
		builtFlakes: make(map[string]bool),
	}
}

// SolutionPaths contains the paths to the built binaries
type SolutionPaths struct {
	Language    string
	Part1       string // Binary path
	Part2       string // Binary path
	SolutionDir string // Directory containing flake.nix
	FlakeDir    string // Directory where the flake was built
	ResultLink  string // Path to the result symlink
}

// FindSolutions finds all solution directories for the given year and day
func (b *Builder) FindSolutions(year, day int) ([]string, error) {
	// Pattern: src/AoC{YY}/s{YY}e{DD} (default) or s{YY}e{DD}-{LANG}
	yearDir := filepath.Join(b.rootDir, fmt.Sprintf("src/AoC%02d", year%100))

	// Check if year directory exists
	if _, err := os.Stat(yearDir); err != nil {
		return nil, fmt.Errorf("year directory not found: %s", yearDir)
	}

	// Pattern for day solutions (with language suffix)
	patternWithLang := fmt.Sprintf("s%02de%02d-*", year%100, day)
	// Exact name for default solution (no language suffix)
	defaultName := fmt.Sprintf("s%02de%02d", year%100, day)

	// Find all matching directories
	entries, err := os.ReadDir(yearDir)
	if err != nil {
		return nil, fmt.Errorf("failed to read year directory: %w", err)
	}

	var solutions []string
	for _, entry := range entries {
		if entry.IsDir() {
			// Check for default implementation (exact match)
			if entry.Name() == defaultName {
				solutions = append(solutions, filepath.Join(yearDir, entry.Name()))
				continue
			}
			// Check for language-specific implementation (pattern match)
			matched, _ := filepath.Match(patternWithLang, entry.Name())
			if matched {
				solutions = append(solutions, filepath.Join(yearDir, entry.Name()))
			}
		}
	}

	if len(solutions) == 0 {
		return nil, fmt.Errorf("no solutions found for year %d day %d", year, day)
	}

	return solutions, nil
}

// FindSolution finds a solution directory for the given year and day
// Deprecated: Use FindSolutions for multi-language support
func (b *Builder) FindSolution(year, day int) (string, error) {
	solutions, err := b.FindSolutions(year, day)
	if err != nil {
		return "", err
	}
	return solutions[0], nil
}

// ExtractLanguage extracts the language from a solution path
// Returns "unknown" if no language can be detected
// Returns the language name if present (e.g., "s16e01-go" -> "go")
func (b *Builder) ExtractLanguage(solutionPath string) string {
	baseName := filepath.Base(solutionPath)

	// Check if there's a hyphen in the name
	idx := len(baseName) - 1
	for idx >= 0 && baseName[idx] != '-' {
		idx--
	}

	// If hyphen found, return language suffix
	if idx >= 0 {
		return baseName[idx+1:]
	}

	// No hyphen found, try to detect language from directory contents
	// First check the solution directory itself
	if lang := b.detectLanguageFromDir(solutionPath); lang != "" {
		return lang
	}

	// Then check the parent directory (for year-level flakes)
	parentDir := filepath.Dir(solutionPath)
	if lang := b.detectLanguageFromDir(parentDir); lang != "" {
		return lang
	}

	return "unknown"
}

// detectLanguageFromDir detects the language by checking for characteristic files
func (b *Builder) detectLanguageFromDir(dir string) string {
	// Check for package.json (TypeScript/JavaScript)
	if _, err := os.Stat(filepath.Join(dir, "package.json")); err == nil {
		return "typescript"
	}

	// Check for Cargo.toml (Rust)
	if _, err := os.Stat(filepath.Join(dir, "Cargo.toml")); err == nil {
		return "rust"
	}

	// Check for go.mod (Go)
	if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
		return "go"
	}

	// Check for *.cabal (Haskell)
	entries, err := os.ReadDir(dir)
	if err == nil {
		for _, entry := range entries {
			if !entry.IsDir() {
				name := entry.Name()
				if filepath.Ext(name) == ".cabal" {
					return "haskell"
				}
			}
		}
	}

	return ""
}

// findFlakeForSolution finds the best flake to use for building a solution
// Returns the flake directory and whether it's a solution-level flake
func (b *Builder) findFlakeForSolution(year, day int, solutionPath string) (string, bool, error) {
	// Priority order:
	// 1. Solution-level flake (e.g., src/AoC16/s16e01-go/flake.nix)
	// 2. Year-level flake (e.g., src/AoC23/flake.nix)
	// 3. Root-level flake (./flake.nix)

	// Check solution-level flake
	if solutionPath != "" {
		solutionFlake := filepath.Join(solutionPath, "flake.nix")
		if _, err := os.Stat(solutionFlake); err == nil {
			return solutionPath, true, nil
		}
	}

	// Check year-level flake
	yearDir := filepath.Join(b.rootDir, fmt.Sprintf("src/AoC%02d", year%100))
	yearFlake := filepath.Join(yearDir, "flake.nix")
	if _, err := os.Stat(yearFlake); err == nil {
		return yearDir, false, nil
	}

	// Check root-level flake
	rootFlake := filepath.Join(b.rootDir, "flake.nix")
	if _, err := os.Stat(rootFlake); err == nil {
		return b.rootDir, false, nil
	}

	return "", false, fmt.Errorf("no flake found for year %d day %d", year, day)
}

// BuildSolution builds a specific solution at the given path
func (b *Builder) BuildSolution(solutionPath string) (*SolutionPaths, error) {
	return b.BuildSolutionForDay(0, 0, solutionPath)
}

// BuildSolutionForDay builds a specific solution for a given year and day
func (b *Builder) BuildSolutionForDay(year, day int, solutionPath string) (*SolutionPaths, error) {
	// Extract solution name from path (e.g., "s16e01-go")
	solutionName := filepath.Base(solutionPath)
	language := b.ExtractLanguage(solutionPath)

	// Check if nix is available
	_, err := exec.LookPath("nix")
	if err == nil {
		// Find the appropriate flake
		flakeDir, isSolutionLevel, err := b.findFlakeForSolution(year, day, solutionPath)
		if err != nil {
			return nil, err
		}

		// Try building with nix
		paths, err := b.buildWithNix(year, day, solutionPath, solutionName, flakeDir, isSolutionLevel)
		if err != nil {
			return nil, err
		}
		paths.Language = language
		return paths, nil
	}

	// Fallback to language-specific build
	fmt.Printf("Nix not found, falling back to language-specific build...\n")
	paths, err := b.buildWithLanguage(solutionPath, solutionName, language)
	if err != nil {
		return nil, err
	}
	paths.Language = language
	return paths, nil
}

// Build builds the solution and returns paths to the binaries
// Deprecated: Use BuildSolution for explicit control
func (b *Builder) Build(year, day int) (*SolutionPaths, error) {
	solutionPath, err := b.FindSolution(year, day)
	if err != nil {
		return nil, err
	}
	return b.BuildSolution(solutionPath)
}

// buildWithNix builds using nix and returns paths to binaries in result/bin
func (b *Builder) buildWithNix(year, day int, solutionPath, solutionName, flakeDir string, isSolutionLevel bool) (*SolutionPaths, error) {
	// Test that the flake is valid by checking if we can evaluate it
	cmd := exec.Command("nix", "flake", "metadata", "--json")
	cmd.Dir = flakeDir
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("failed to evaluate flake: %w", err)
	}

	// Use a unique result link based on the solution path to avoid conflicts
	// when building multiple solutions in parallel or sequentially
	resultLink := filepath.Join(b.rootDir, ".aoc-build", solutionName)

	// Determine binary names based on whether it's a solution-level or year-level flake
	var part1Binary, part2Binary string
	if isSolutionLevel {
		part1Binary = fmt.Sprintf("%s-part1", solutionName)
		part2Binary = fmt.Sprintf("%s-part2", solutionName)
	} else {
		part1Binary = fmt.Sprintf("s%02de%02d-part1", year%100, day)
		part2Binary = fmt.Sprintf("s%02de%02d-part2", year%100, day)
	}

	// Check if binaries already exist and we've already built this flake
	part1Path := filepath.Join(resultLink, "bin", part1Binary)
	part2Path := filepath.Join(resultLink, "bin", part2Binary)

	_, part1Exists := os.Stat(part1Path)
	_, part2Exists := os.Stat(part2Path)
	alreadyBuilt := b.builtFlakes[flakeDir]

	if part1Exists == nil && part2Exists == nil && alreadyBuilt {
		// Binaries exist and we've built this flake - use cached build
		// Silent - no output
	} else {
		// Build the flake
		if err := os.MkdirAll(filepath.Dir(resultLink), 0755); err != nil {
			return nil, fmt.Errorf("failed to create build directory: %w", err)
		}

		cmd = exec.Command("nix", "build", "--out-link", resultLink)
		cmd.Dir = flakeDir
		output, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("failed to build flake: %w\nOutput: %s", err, output)
		}

		// Mark this flake as built
		b.builtFlakes[flakeDir] = true
	}

	// Verify binaries exist
	if _, err := os.Stat(part1Path); err != nil {
		return nil, fmt.Errorf("part1 binary not found: %s", part1Path)
	}
	if _, err := os.Stat(part2Path); err != nil {
		return nil, fmt.Errorf("part2 binary not found: %s", part2Path)
	}

	return &SolutionPaths{
		Part1:       part1Path,
		Part2:       part2Path,
		SolutionDir: solutionPath,
		FlakeDir:    flakeDir,
		ResultLink:  resultLink,
	}, nil
}

// buildWithLanguage builds using language-specific tools
func (b *Builder) buildWithLanguage(solutionPath, solutionName, language string) (*SolutionPaths, error) {
	switch language {
	case "go":
		return b.buildGo(solutionPath, solutionName)
	case "rust":
		return b.buildRust(solutionPath, solutionName)
	default:
		return nil, fmt.Errorf("language %s not supported for non-Nix builds", language)
	}
}

// buildGo builds using Go directly
func (b *Builder) buildGo(solutionPath, solutionName string) (*SolutionPaths, error) {
	// Create build directory
	buildDir := filepath.Join(solutionPath, ".build")
	if err := os.MkdirAll(buildDir, 0755); err != nil {
		return nil, fmt.Errorf("failed to create build directory: %w", err)
	}

	paths := &SolutionPaths{
		Part1: filepath.Join(buildDir, fmt.Sprintf("%s-part1", solutionName)),
		Part2: filepath.Join(buildDir, fmt.Sprintf("%s-part2", solutionName)),
	}

	// Build part1
	cmd := exec.Command("go", "build", "-o", paths.Part1, fmt.Sprintf("./cmd/%s-part1", solutionName))
	cmd.Dir = solutionPath
	if output, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("failed to build part1: %w\nOutput: %s", err, output)
	}

	// Build part2
	cmd = exec.Command("go", "build", "-o", paths.Part2, fmt.Sprintf("./cmd/%s-part2", solutionName))
	cmd.Dir = solutionPath
	if output, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("failed to build part2: %w\nOutput: %s", err, output)
	}

	// Verify binaries exist
	if _, err := os.Stat(paths.Part1); err != nil {
		return nil, fmt.Errorf("part1 binary not found: %w", err)
	}
	if _, err := os.Stat(paths.Part2); err != nil {
		return nil, fmt.Errorf("part2 binary not found: %w", err)
	}

	fmt.Printf("Built successfully with Go\n")
	return paths, nil
}

// buildRust builds using Cargo directly
func (b *Builder) buildRust(solutionPath, solutionName string) (*SolutionPaths, error) {
	// Build with cargo
	cmd := exec.Command("cargo", "build", "--release")
	cmd.Dir = solutionPath
	if output, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("failed to build with cargo: %w\nOutput: %s", err, output)
	}

	paths := &SolutionPaths{
		Part1: filepath.Join(solutionPath, "target/release", fmt.Sprintf("%s-part1", solutionName)),
		Part2: filepath.Join(solutionPath, "target/release", fmt.Sprintf("%s-part2", solutionName)),
	}

	// Verify binaries exist
	if _, err := os.Stat(paths.Part1); err != nil {
		return nil, fmt.Errorf("part1 binary not found: %w", err)
	}
	if _, err := os.Stat(paths.Part2); err != nil {
		return nil, fmt.Errorf("part2 binary not found: %w", err)
	}

	fmt.Printf("Built successfully with Rust\n")
	return paths, nil
}
