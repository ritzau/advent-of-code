package common

import "strings"

func ParseInput(input string) []string {
	return strings.Split(strings.TrimSpace(input), "\n")
}
