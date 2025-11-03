package downloader

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// Downloader handles downloading AoC inputs
type Downloader struct {
	rootDir string
}

// New creates a new Downloader
func New(rootDir string) *Downloader {
	return &Downloader{rootDir: rootDir}
}

// GetInput downloads (or returns cached) input for the given year and day
func (d *Downloader) GetInput(year, day int) (string, error) {
	// Check if input already exists in cache
	inputPath := filepath.Join(d.rootDir, "inputs", fmt.Sprintf("%d", year), fmt.Sprintf("day%02d.txt", day))

	// If it exists, return the path
	if _, err := os.Stat(inputPath); err == nil {
		return inputPath, nil
	}

	// Otherwise, download it using the Python script
	scriptPath := filepath.Join(d.rootDir, "scripts", "download-input.py")

	cmd := exec.Command("python3", scriptPath, fmt.Sprintf("%d", year), fmt.Sprintf("%d", day))
	cmd.Dir = d.rootDir
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Run(); err != nil {
		return "", fmt.Errorf("failed to download input: %w", err)
	}

	// Verify the file was created
	if _, err := os.Stat(inputPath); err != nil {
		return "", fmt.Errorf("input file not created: %w", err)
	}

	return inputPath, nil
}

// ReadInput reads the input file and returns its contents
func (d *Downloader) ReadInput(year, day int) (string, error) {
	inputPath, err := d.GetInput(year, day)
	if err != nil {
		return "", err
	}

	data, err := os.ReadFile(inputPath)
	if err != nil {
		return "", fmt.Errorf("failed to read input file: %w", err)
	}

	return string(data), nil
}
