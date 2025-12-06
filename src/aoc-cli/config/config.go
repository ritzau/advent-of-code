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
	Skip          interface{}               `yaml:"skip"` // Can be bool or []string (list of languages to skip)
	Part1         interface{}               `yaml:"part1"`
	Part2         interface{}               `yaml:"part2"`
	Results       []interface{}             `yaml:"results"` // Alternative to part1/part2
	Languages     map[string]LanguageConfig `yaml:"languages"`
	skipAll       bool                      // Cached: true if skip is boolean true
	skipLanguages map[string]bool           // Cached: set of languages to skip
}

// UnmarshalYAML implements custom unmarshaling to support both array and object syntax
func (dc *DayConfig) UnmarshalYAML(node *yaml.Node) error {
	// Handle array syntax: [part1, part2]
	if node.Kind == yaml.SequenceNode {
		var parts []interface{}
		if err := node.Decode(&parts); err != nil {
			return err
		}

		if len(parts) > 0 {
			dc.Part1 = parts[0]
		}
		if len(parts) > 1 {
			dc.Part2 = parts[1]
		}
		dc.processSkip()
		return nil
	}

	// Handle object syntax
	// Use a temporary struct to avoid infinite recursion
	type dayConfigTemp struct {
		Skip      interface{}               `yaml:"skip"`
		Part1     interface{}               `yaml:"part1"`
		Part2     interface{}               `yaml:"part2"`
		Results   []interface{}             `yaml:"results"`
		Languages map[string]LanguageConfig `yaml:"languages"`
	}

	var temp dayConfigTemp
	if err := node.Decode(&temp); err != nil {
		return err
	}

	dc.Skip = temp.Skip
	dc.Part1 = temp.Part1
	dc.Part2 = temp.Part2
	dc.Results = temp.Results
	dc.Languages = temp.Languages

	// If results array is provided, use it to set part1/part2
	if len(dc.Results) > 0 {
		if dc.Part1 == nil && len(dc.Results) > 0 {
			dc.Part1 = dc.Results[0]
		}
		if dc.Part2 == nil && len(dc.Results) > 1 {
			dc.Part2 = dc.Results[1]
		}
	}

	dc.processSkip()
	return nil
}

// processSkip processes the skip field and caches the results
func (dc *DayConfig) processSkip() {
	dc.skipLanguages = make(map[string]bool)

	if dc.Skip == nil {
		return
	}

	// Handle boolean skip
	if skipBool, ok := dc.Skip.(bool); ok {
		dc.skipAll = skipBool
		return
	}

	// Handle string array skip (list of languages)
	if skipList, ok := dc.Skip.([]interface{}); ok {
		for _, lang := range skipList {
			if langStr, ok := lang.(string); ok {
				dc.skipLanguages[langStr] = true
			}
		}
	}
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

// ShouldSkip returns true if the day should be skipped entirely
func (r *Results) ShouldSkip(year, day int) bool {
	dayConfig, exists := r.GetDayConfig(year, day)
	if !exists {
		return false
	}
	return dayConfig.skipAll
}

// ShouldSkipLanguage returns true if the specific language should be skipped for this day
func (r *Results) ShouldSkipLanguage(year, day int, language string) bool {
	dayConfig, exists := r.GetDayConfig(year, day)
	if !exists {
		return false
	}

	// Check if the entire day is skipped
	if dayConfig.skipAll {
		return true
	}

	// Check if this specific language should be skipped
	if dayConfig.skipLanguages[language] {
		return true
	}

	// Check language-specific skip in the old format
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
