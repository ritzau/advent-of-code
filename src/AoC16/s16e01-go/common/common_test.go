package common

import "testing"

func TestSolvePart1Sample1(t *testing.T) {
	// Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away
	result := SolvePart1("R2, L3")
	expected := 5
	if result != expected {
		t.Errorf("SolvePart1(\"R2, L3\") = %d; want %d", result, expected)
	}
}

func TestSolvePart1Sample2(t *testing.T) {
	// R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away
	result := SolvePart1("R2, R2, R2")
	expected := 2
	if result != expected {
		t.Errorf("SolvePart1(\"R2, R2, R2\") = %d; want %d", result, expected)
	}
}

func TestSolvePart1Sample3(t *testing.T) {
	// R5, L5, R5, R3 leaves you 12 blocks away
	result := SolvePart1("R5, L5, R5, R3")
	expected := 12
	if result != expected {
		t.Errorf("SolvePart1(\"R5, L5, R5, R3\") = %d; want %d", result, expected)
	}
}

func TestSolvePart2Sample1(t *testing.T) {
	// R8, R4, R4, R8 - first location visited twice is 4 blocks away, due East
	result := SolvePart2("R8, R4, R4, R8")
	expected := 4
	if result != expected {
		t.Errorf("SolvePart2(\"R8, R4, R4, R8\") = %d; want %d", result, expected)
	}
}
