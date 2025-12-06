package downloader

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"
)

// Downloader handles downloading AoC inputs
type Downloader struct {
	rootDir         string
	lastRequestTime time.Time
	minRequestDelay time.Duration
}

// New creates a new Downloader
func New(rootDir string) *Downloader {
	d := &Downloader{
		rootDir:         rootDir,
		minRequestDelay: time.Minute, // As per automation guidelines: max 1 request per minute
	}
	// Load last request time from cache
	d.loadLastRequestTime()
	return d
}

// loadLastRequestTime loads the last request timestamp from cache file
func (d *Downloader) loadLastRequestTime() {
	timestampFile := filepath.Join(d.rootDir, ".aoc-last-request")
	data, err := os.ReadFile(timestampFile)
	if err != nil {
		// File doesn't exist or can't be read - not an error, just means no previous requests
		return
	}

	timestamp, err := time.Parse(time.RFC3339, strings.TrimSpace(string(data)))
	if err != nil {
		// Invalid timestamp - ignore it
		return
	}

	d.lastRequestTime = timestamp
}

// saveLastRequestTime saves the last request timestamp to cache file
func (d *Downloader) saveLastRequestTime() {
	timestampFile := filepath.Join(d.rootDir, ".aoc-last-request")
	timestamp := d.lastRequestTime.Format(time.RFC3339)
	// Ignore errors - not critical if we can't save
	_ = os.WriteFile(timestampFile, []byte(timestamp), 0o644)
}

// getSessionCookie reads the session cookie from AOC_SESSION env variable or .aoc-session file
func (d *Downloader) getSessionCookie() (string, error) {
	// First, try to read from environment variable
	if session := os.Getenv("AOC_SESSION"); session != "" {
		return strings.TrimSpace(session), nil
	}

	// Fall back to reading from .aoc-session file
	sessionFile := filepath.Join(d.rootDir, ".aoc-session")

	data, err := os.ReadFile(sessionFile)
	if err != nil {
		if os.IsNotExist(err) {
			return "", fmt.Errorf(".aoc-session file not found and AOC_SESSION environment variable not set\nEither:\n  - Set the AOC_SESSION environment variable, or\n  - Create a .aoc-session file with your session cookie from adventofcode.com\nYou can find it in your browser cookies after logging in")
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
	// Rate limiting: ensure we don't make requests more frequently than minRequestDelay
	// As per https://www.reddit.com/r/adventofcode/wiki/faqs/automation/
	if !d.lastRequestTime.IsZero() {
		timeSinceLastRequest := time.Since(d.lastRequestTime)
		if timeSinceLastRequest < d.minRequestDelay {
			waitTime := d.minRequestDelay - timeSinceLastRequest
			fmt.Fprintf(os.Stderr, "Rate limiting: waiting %v before making request...\n", waitTime.Round(time.Second))
			time.Sleep(waitTime)
		}
	}

	session, err := d.getSessionCookie()
	if err != nil {
		return "", err
	}

	url := fmt.Sprintf("https://adventofcode.com/%d/day/%d/input", year, day)

	// Create HTTP request
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return "", fmt.Errorf("failed to create request: %w", err)
	}

	// Add headers
	req.Header.Set("Cookie", fmt.Sprintf("session=%s", session))
	// User-Agent as per https://www.reddit.com/r/adventofcode/wiki/faqs/automation/
	req.Header.Set("User-Agent", "github.com/ritzau/advent-of-code by https://github.com/ritzau")

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

	// Update last request time for rate limiting
	d.lastRequestTime = time.Now()
	d.saveLastRequestTime()

	return string(body), nil
}

// GetInput downloads (or returns cached) input for the given year and day
func (d *Downloader) GetInput(year, day int) (string, error) {
	// Check if input already exists in cache
	inputPath := filepath.Join(d.rootDir, "inputs", fmt.Sprintf("%d", year), fmt.Sprintf("day%02d.txt", day))

	// If it exists, return the path
	if _, err := os.Stat(inputPath); err == nil {
		return inputPath, nil
	}

	// Download the input
	content, err := d.downloadInput(year, day)
	if err != nil {
		return "", err
	}

	// Create cache directory
	cacheDir := filepath.Join(d.rootDir, "inputs", fmt.Sprintf("%d", year))
	if err := os.MkdirAll(cacheDir, 0o755); err != nil {
		return "", fmt.Errorf("failed to create cache directory: %w", err)
	}

	// Save to cache
	if err := os.WriteFile(inputPath, []byte(content), 0o644); err != nil {
		return "", fmt.Errorf("failed to save input to cache: %w", err)
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
