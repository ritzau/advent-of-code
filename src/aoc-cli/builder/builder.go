package builder

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// Builder handles building AoC solutions
type Builder struct {
	rootDir string
}

// New creates a new Builder
func New(rootDir string) *Builder {
	return &Builder{rootDir: rootDir}
}

// SolutionPaths contains the paths to the built binaries or nix run info
type SolutionPaths struct {
	Language    string
	Part1       string // Binary path or "nix-run"
	Part2       string // Binary path or "nix-run"
	SolutionDir string // Directory containing flake.nix for nix run
	UseNixRun   bool   // If true, use nix run instead of binaries
}

// FindSolutions finds all solution directories for the given year and day
func (b *Builder) FindSolutions(year, day int) ([]string, error) {
	// Pattern: src/AoC{YY}/s{YY}e{DD}-{LANG}
	yearDir := filepath.Join(b.rootDir, fmt.Sprintf("src/AoC%02d", year%100))

	// Check if year directory exists
	if _, err := os.Stat(yearDir); err != nil {
		return nil, fmt.Errorf("year directory not found: %s", yearDir)
	}

	// Pattern for day solutions
	pattern := fmt.Sprintf("s%02de%02d-*", year%100, day)

	// Find all matching directories
	entries, err := os.ReadDir(yearDir)
	if err != nil {
		return nil, fmt.Errorf("failed to read year directory: %w", err)
	}

	var solutions []string
	for _, entry := range entries {
		if entry.IsDir() {
			matched, _ := filepath.Match(pattern, entry.Name())
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
func (b *Builder) ExtractLanguage(solutionPath string) string {
	// Extract from path like "s16e01-go" -> "go"
	baseName := filepath.Base(solutionPath)
	parts := filepath.SplitList(baseName)
	if len(parts) > 0 {
		lastPart := parts[len(parts)-1]
		// Split by hyphen and take last part
		langParts := filepath.SplitList(lastPart)
		if len(langParts) > 1 {
			return langParts[len(langParts)-1]
		}
	}
	// Fallback: just get everything after last hyphen
	idx := len(baseName) - 1
	for idx >= 0 && baseName[idx] != '-' {
		idx--
	}
	if idx >= 0 {
		return baseName[idx+1:]
	}
	return "unknown"
}

// BuildSolution builds a specific solution at the given path
func (b *Builder) BuildSolution(solutionPath string) (*SolutionPaths, error) {
	fmt.Printf("Building solution at %s...\n", solutionPath)

	// Extract solution name from path (e.g., "s16e01-go")
	solutionName := filepath.Base(solutionPath)
	language := b.ExtractLanguage(solutionPath)

	// Check if nix is available
	_, err := exec.LookPath("nix")
	if err == nil {
		// Try building with nix
		paths, err := b.buildWithNix(solutionPath, solutionName)
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

// buildWithNix prepares for running via nix run
func (b *Builder) buildWithNix(solutionPath, solutionName string) (*SolutionPaths, error) {
	// Test that the flake is valid by checking if we can evaluate it
	cmd := exec.Command("nix", "flake", "metadata", "--json")
	cmd.Dir = solutionPath
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("failed to evaluate flake: %w", err)
	}

	fmt.Printf("Solution has valid flake, will use nix run\n")

	// Return paths configured for nix run
	paths := &SolutionPaths{
		Part1:       "part1", // App name in the flake
		Part2:       "part2", // App name in the flake
		SolutionDir: solutionPath,
		UseNixRun:   true,
	}

	return paths, nil
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
