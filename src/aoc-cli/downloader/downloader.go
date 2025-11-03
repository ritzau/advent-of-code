package downloader

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
)

// Downloader handles downloading AoC inputs
type Downloader struct {
	rootDir string
}

// New creates a new Downloader
func New(rootDir string) *Downloader {
	return &Downloader{rootDir: rootDir}
}

// getSessionCookie reads the session cookie from .aoc-session file
func (d *Downloader) getSessionCookie() (string, error) {
	sessionFile := filepath.Join(d.rootDir, ".aoc-session")

	data, err := os.ReadFile(sessionFile)
	if err != nil {
		if os.IsNotExist(err) {
			return "", fmt.Errorf(".aoc-session file not found\nCreate a .aoc-session file with your session cookie from adventofcode.com\nYou can find it in your browser cookies after logging in")
		}
		return "", fmt.Errorf("failed to read .aoc-session file: %w", err)
	}

	session := strings.TrimSpace(string(data))
	if session == "" {
		return "", fmt.Errorf(".aoc-session file is empty")
	}

	return session, nil
}

// downloadInput downloads the input from adventofcode.com
func (d *Downloader) downloadInput(year, day int) (string, error) {
	session, err := d.getSessionCookie()
	if err != nil {
		return "", err
	}

	url := fmt.Sprintf("https://adventofcode.com/%d/day/%d/input", year, day)
	fmt.Printf("Downloading from %s...\n", url)

	// Create HTTP request
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return "", fmt.Errorf("failed to create request: %w", err)
	}

	// Add headers
	req.Header.Set("Cookie", fmt.Sprintf("session=%s", session))
	req.Header.Set("User-Agent", "github.com/ritzau/advent-of-code by contact@example.com")

	// Make request
	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return "", fmt.Errorf("failed to fetch input: %w", err)
	}
	defer resp.Body.Close()

	// Check status code
	if resp.StatusCode != http.StatusOK {
		switch resp.StatusCode {
		case http.StatusNotFound:
			return "", fmt.Errorf("input not available yet for %d day %d", year, day)
		case http.StatusBadRequest:
			return "", fmt.Errorf("invalid session cookie")
		default:
			return "", fmt.Errorf("HTTP %d: %s", resp.StatusCode, resp.Status)
		}
	}

	// Read response
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return "", fmt.Errorf("failed to read response: %w", err)
	}

	return string(body), nil
}

// GetInput downloads (or returns cached) input for the given year and day
func (d *Downloader) GetInput(year, day int) (string, error) {
	// Check if input already exists in cache
	inputPath := filepath.Join(d.rootDir, "inputs", fmt.Sprintf("%d", year), fmt.Sprintf("day%02d.txt", day))

	// If it exists, return the path
	if _, err := os.Stat(inputPath); err == nil {
		fmt.Printf("Using cached input: %s\n", inputPath)
		return inputPath, nil
	}

	// Download the input
	content, err := d.downloadInput(year, day)
	if err != nil {
		return "", err
	}

	// Create cache directory
	cacheDir := filepath.Join(d.rootDir, "inputs", fmt.Sprintf("%d", year))
	if err := os.MkdirAll(cacheDir, 0755); err != nil {
		return "", fmt.Errorf("failed to create cache directory: %w", err)
	}

	// Save to cache
	if err := os.WriteFile(inputPath, []byte(content), 0644); err != nil {
		return "", fmt.Errorf("failed to save input to cache: %w", err)
	}

	fmt.Printf("Downloaded and cached: %s\n", inputPath)
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
