package common

import "testing"

const sample_input string = `0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2`

func TestSolvePart1Sample1(t *testing.T) {
	result := SolvePart1(sample_input)
	expected := 2
	if result != expected {
		t.Errorf("SolvePart1(%q) = %d; want %d", "", result, expected)
	}
}

func TestSolvePart2Sample1(t *testing.T) {
	result := SolvePart2(sample_input)
	expected := 0 // TODO: Add expected result
	if result != expected {
		t.Errorf("SolvePart2(%q) = %d; want %d", "", result, expected)
	}
}
