package builder

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// Builder handles building AoC solutions
type Builder struct {
	rootDir      string
	builtTargets map[string]bool // Track which targets have been built in this session
}

// New creates a new Builder
func New(rootDir string) *Builder {
	return &Builder{
		rootDir:      rootDir,
		builtTargets: make(map[string]bool),
	}
}

// SolutionPaths contains the paths to the built binaries
type SolutionPaths struct {
	Language    string
	Part1       string // Binary path
	Part2       string // Binary path
	SolutionDir string // Directory containing BUILD.bazel
	BuildDir    string // Bazel build directory
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

	// Then check the parent directory (for year-level solutions)
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

// findBazelTarget finds the Bazel target for a solution
// Returns the workspace-relative path and target name
func (b *Builder) findBazelTarget(year, day int, solutionPath string) (string, string, error) {
	// Convert absolute path to workspace-relative path
	relPath, err := filepath.Rel(b.rootDir, solutionPath)
	if err != nil {
		return "", "", fmt.Errorf("failed to get relative path: %w", err)
	}

	// Check if BUILD.bazel exists in the solution directory
	buildFile := filepath.Join(solutionPath, "BUILD.bazel")
	if _, err := os.Stat(buildFile); err != nil {
		// Try checking in src/ subdirectory (for C++ and similar)
		buildFile = filepath.Join(solutionPath, "src", "BUILD.bazel")
		if _, err := os.Stat(buildFile); err != nil {
			return "", "", fmt.Errorf("no BUILD.bazel found in %s or %s/src", solutionPath, solutionPath)
		}
		// Use the src subdirectory
		relPath = filepath.Join(relPath, "src")
	}

	// Construct Bazel package path (using //)
	pkgPath := "//" + strings.ReplaceAll(relPath, string(filepath.Separator), "/")

	// Extract solution name from path
	solutionName := filepath.Base(solutionPath)

	return pkgPath, solutionName, nil
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

	// Check if bazel is available
	_, err := exec.LookPath("bazel")
	if err == nil {
		// Build with Bazel
		paths, err := b.buildWithBazel(year, day, solutionPath, solutionName)
		if err != nil {
			return nil, err
		}
		paths.Language = language
		return paths, nil
	}

	// Fallback to language-specific build
	fmt.Printf("Bazel not found, falling back to language-specific build...\n")
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

// buildWithBazel builds using Bazel and returns paths to binaries
func (b *Builder) buildWithBazel(year, day int, solutionPath, solutionName string) (*SolutionPaths, error) {
	// Find the Bazel target
	pkgPath, _, err := b.findBazelTarget(year, day, solutionPath)
	if err != nil {
		return nil, err
	}

	// Use standardized target names: part1 and part2
	part1Target := fmt.Sprintf("%s:part1", pkgPath)
	part2Target := fmt.Sprintf("%s:part2", pkgPath)

	// Check if we've already built these targets
	targetKey := fmt.Sprintf("%s+%s", part1Target, part2Target)
	if !b.builtTargets[targetKey] {
		// Build both targets
		cmd := exec.Command("bazel", "build", part1Target, part2Target)
		cmd.Dir = b.rootDir
		output, err := cmd.CombinedOutput()
		if err != nil {
			return nil, fmt.Errorf("failed to build Bazel targets: %w\nOutput: %s", err, output)
		}

		// Mark as built
		b.builtTargets[targetKey] = true
	}

	// Query Bazel for the output paths
	part1Path, err := b.queryBazelOutput(part1Target)
	if err != nil {
		return nil, fmt.Errorf("failed to get part1 binary path: %w", err)
	}

	part2Path, err := b.queryBazelOutput(part2Target)
	if err != nil {
		return nil, fmt.Errorf("failed to get part2 binary path: %w", err)
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
		BuildDir:    filepath.Join(b.rootDir, "bazel-bin"),
	}, nil
}

// queryBazelOutput queries Bazel for the output path of a target
func (b *Builder) queryBazelOutput(target string) (string, error) {
	cmd := exec.Command("bazel", "cquery", "--output=files", "--color=no", target)
	cmd.Dir = b.rootDir
	output, err := cmd.Output()
	if err != nil {
		return "", fmt.Errorf("failed to query Bazel output: %w", err)
	}

	// The output may contain multiple lines (e.g., binary + clippy check file)
	// Find the first line that doesn't end with a known check extension
	lines := strings.Split(strings.TrimSpace(string(output)), "\n")
	var jarFile string
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		// Skip dependency and check files
		if strings.HasSuffix(line, ".jdeps") || strings.HasSuffix(line, ".clippy.ok") || strings.HasSuffix(line, ".rustfmt.ok") {
			continue
		}

		// For JVM binaries (JAR files), we need to use the wrapper script instead
		if strings.HasSuffix(line, ".jar") {
			jarFile = line
			continue
		}

		// Convert to absolute path if needed
		if !filepath.IsAbs(line) {
			line = filepath.Join(b.rootDir, line)
		}

		return line, nil
	}

	// If we only found a JAR file, return the wrapper script path
	if jarFile != "" {
		// Remove .jar extension and convert to absolute path
		scriptPath := strings.TrimSuffix(jarFile, ".jar")
		if !filepath.IsAbs(scriptPath) {
			scriptPath = filepath.Join(b.rootDir, scriptPath)
		}
		return scriptPath, nil
	}

	return "", fmt.Errorf("no valid binary found in bazel cquery output")
}

// buildWithLanguage builds using language-specific tools
func (b *Builder) buildWithLanguage(solutionPath, solutionName, language string) (*SolutionPaths, error) {
	switch language {
	case "go":
		return b.buildGo(solutionPath, solutionName)
	case "rust":
		return b.buildRust(solutionPath, solutionName)
	default:
		return nil, fmt.Errorf("language %s not supported for non-Bazel builds", language)
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
