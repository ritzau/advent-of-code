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

// SolutionPaths contains the paths to the built binaries
type SolutionPaths struct {
	Part1 string
	Part2 string
}

// FindSolution finds a solution directory for the given year and day
func (b *Builder) FindSolution(year, day int) (string, error) {
	// Try different patterns
	patterns := []string{
		fmt.Sprintf("src/AoC%02d/s%02de%02d-go", year%100, year%100, day),
		fmt.Sprintf("src/AoC%d/s%02de%02d-go", year, year%100, day),
	}

	for _, pattern := range patterns {
		solutionPath := filepath.Join(b.rootDir, pattern)
		if _, err := os.Stat(solutionPath); err == nil {
			return solutionPath, nil
		}
	}

	return "", fmt.Errorf("no solution found for year %d day %d", year, day)
}

// Build builds the solution and returns paths to the binaries
func (b *Builder) Build(year, day int) (*SolutionPaths, error) {
	solutionPath, err := b.FindSolution(year, day)
	if err != nil {
		return nil, err
	}

	fmt.Printf("Building solution at %s...\n", solutionPath)

	// Check if nix is available
	_, err = exec.LookPath("nix")
	if err == nil {
		// Try building with nix
		return b.buildWithNix(solutionPath)
	}

	// Fallback to Go build
	fmt.Printf("Nix not found, falling back to Go build...\n")
	return b.buildWithGo(solutionPath)
}

// buildWithNix builds using Nix
func (b *Builder) buildWithNix(solutionPath string) (*SolutionPaths, error) {
	cmd := exec.Command("nix", "build", "--no-link", "--print-out-paths")
	cmd.Dir = solutionPath
	output, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("failed to build solution with nix: %w", err)
	}

	outPath := string(output)
	if len(outPath) > 0 && outPath[len(outPath)-1] == '\n' {
		outPath = outPath[:len(outPath)-1]
	}

	paths := &SolutionPaths{
		Part1: filepath.Join(outPath, "bin", "part1"),
		Part2: filepath.Join(outPath, "bin", "part2"),
	}

	// Verify binaries exist
	if _, err := os.Stat(paths.Part1); err != nil {
		return nil, fmt.Errorf("part1 binary not found: %w", err)
	}
	if _, err := os.Stat(paths.Part2); err != nil {
		return nil, fmt.Errorf("part2 binary not found: %w", err)
	}

	return paths, nil
}

// buildWithGo builds using Go directly
func (b *Builder) buildWithGo(solutionPath string) (*SolutionPaths, error) {
	// Create build directory
	buildDir := filepath.Join(solutionPath, ".build")
	if err := os.MkdirAll(buildDir, 0755); err != nil {
		return nil, fmt.Errorf("failed to create build directory: %w", err)
	}

	paths := &SolutionPaths{
		Part1: filepath.Join(buildDir, "part1"),
		Part2: filepath.Join(buildDir, "part2"),
	}

	// Build part1
	cmd := exec.Command("go", "build", "-o", paths.Part1, "./cmd/part1")
	cmd.Dir = solutionPath
	if output, err := cmd.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("failed to build part1: %w\nOutput: %s", err, output)
	}

	// Build part2
	cmd = exec.Command("go", "build", "-o", paths.Part2, "./cmd/part2")
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
