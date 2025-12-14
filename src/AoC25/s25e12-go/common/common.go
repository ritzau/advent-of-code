package common

import (
	"fmt"
	"strconv"
	"strings"
)

// Region represents a rectangular area with package counts
type Region struct {
	Width         int
	Height        int
	PackageCounts []int
}

// ParseInput parses the input into packages and regions
func ParseInput(input string) ([][][]bool, []Region) {
	lines := strings.Split(strings.TrimSpace(input), "\n")

	var packages [][][]bool

	i := 0
	expectedID := 0
	for i < len(lines) {
		header := lines[i]
		if !strings.HasSuffix(header, ":") {
			break
		}

		num, err := strconv.Atoi(header[:len(header)-1])
		if err != nil || num != expectedID {
			break
		}
		expectedID++
		i++

		var pkg [][]bool
		for i < len(lines) && lines[i] != "" {
			line := lines[i]
			row := make([]bool, len(line))
			for j, r := range line {
				row[j] = r == '#'
			}
			pkg = append(pkg, row)
			i++
		}
		packages = append(packages, pkg)

		i++ // Skip empty line
	}

	var regions []Region

	// Continue with remaining lines from index i
	for i < len(lines) {
		head, packageCountStr, _ := strings.Cut(lines[i], ":")
		dimensions := strings.Split(head, "x")
		width, _ := strconv.Atoi(dimensions[0])
		height, _ := strconv.Atoi(dimensions[1])

		var packageCounts []int
		for _, countStr := range strings.Fields(packageCountStr) {
			count, err := strconv.Atoi(countStr)
			if err != nil {
				panic(fmt.Sprintf("unexpected value: %v", countStr))
			}
			packageCounts = append(packageCounts, count)
		}

		regions = append(regions, Region{
			Width:         width,
			Height:        height,
			PackageCounts: packageCounts,
		})

		i++
	}

	return packages, regions
}

func SolvePart1(input string) int {
	packages, regions := ParseInput(input)
	fmt.Printf("Packages: %+v\nRegions: %+v\n", packages, regions)
	return 0
}

func SolvePart2(input string) int {
	packages, regions := ParseInput(input)
	_ = packages
	_ = regions
	return 0
}
