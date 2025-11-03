package config

import (
	"fmt"
	"os"

	"gopkg.in/yaml.v3"
)

// LanguageConfig represents language-specific configuration
type LanguageConfig struct {
	Skip  bool        `yaml:"skip"`
	Part1 interface{} `yaml:"part1"`
	Part2 interface{} `yaml:"part2"`
}

// DayConfig represents the configuration for a single day
type DayConfig struct {
	Skip      bool                       `yaml:"skip"`
	Part1     interface{}                `yaml:"part1"`
	Part2     interface{}                `yaml:"part2"`
	Languages map[string]LanguageConfig  `yaml:"languages"`
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

// ShouldSkipLanguage returns true if the specific language should be skipped for this day
func (r *Results) ShouldSkipLanguage(year, day int, language string) bool {
	dayConfig, exists := r.GetDayConfig(year, day)
	if !exists {
		return false
	}

	// Check day-level skip first
	if dayConfig.Skip {
		return true
	}

	// Check language-specific skip
	if langConfig, ok := dayConfig.Languages[language]; ok {
		return langConfig.Skip
	}

	return false
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

// GetExpectedResultForLanguage returns the expected result for a part and language
// Falls back to day-level expected result if no language-specific result is defined
func (r *Results) GetExpectedResultForLanguage(year, day, part int, language string) (string, bool) {
	dayConfig, exists := r.GetDayConfig(year, day)
	if !exists {
		return "", false
	}

	// Check for language-specific result first
	if langConfig, ok := dayConfig.Languages[language]; ok {
		var result interface{}
		if part == 1 {
			result = langConfig.Part1
		} else if part == 2 {
			result = langConfig.Part2
		}

		if result != nil {
			return fmt.Sprintf("%v", result), true
		}
	}

	// Fall back to day-level result
	return r.GetExpectedResult(year, day, part)
}

// HasExpectedResults returns true if the day has any expected results
func (r *Results) HasExpectedResults(year, day int) bool {
	dayConfig, exists := r.GetDayConfig(year, day)
	if !exists {
		return false
	}
	return dayConfig.Part1 != nil || dayConfig.Part2 != nil
}
