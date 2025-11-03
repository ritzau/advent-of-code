package config

import (
	"fmt"
	"os"

	"gopkg.in/yaml.v3"
)

// DayConfig represents the configuration for a single day
type DayConfig struct {
	Skip  bool        `yaml:"skip"`
	Part1 interface{} `yaml:"part1"`
	Part2 interface{} `yaml:"part2"`
}

// Results represents the entire results configuration
type Results struct {
	Years map[int]map[int]DayConfig
}

// LoadResults loads the results.yaml file
func LoadResults(path string) (*Results, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("failed to read results file: %w", err)
	}

	var years map[int]map[int]DayConfig
	if err := yaml.Unmarshal(data, &years); err != nil {
		return nil, fmt.Errorf("failed to parse results file: %w", err)
	}

	return &Results{Years: years}, nil
}

// GetDayConfig returns the configuration for a specific year and day
func (r *Results) GetDayConfig(year, day int) (DayConfig, bool) {
	yearData, ok := r.Years[year]
	if !ok {
		return DayConfig{}, false
	}

	dayData, ok := yearData[day]
	return dayData, ok
}

// ShouldSkip returns true if the day should be skipped
func (r *Results) ShouldSkip(year, day int) bool {
	dayConfig, exists := r.GetDayConfig(year, day)
	if !exists {
		return false
	}
	return dayConfig.Skip
}

// GetExpectedResult returns the expected result for a part
func (r *Results) GetExpectedResult(year, day, part int) (string, bool) {
	dayConfig, exists := r.GetDayConfig(year, day)
	if !exists {
		return "", false
	}

	var result interface{}
	if part == 1 {
		result = dayConfig.Part1
	} else if part == 2 {
		result = dayConfig.Part2
	}

	if result == nil {
		return "", false
	}

	return fmt.Sprintf("%v", result), true
}

// HasExpectedResults returns true if the day has any expected results
func (r *Results) HasExpectedResults(year, day int) bool {
	dayConfig, exists := r.GetDayConfig(year, day)
	if !exists {
		return false
	}
	return dayConfig.Part1 != nil || dayConfig.Part2 != nil
}
