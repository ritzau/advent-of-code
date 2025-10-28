package common

import (
	"strconv"
	"strings"
)

type Direction int

const (
	North Direction = iota
	East
	South
	West
)

func (d Direction) TurnRight() Direction {
	switch d {
	case North:
		return East
	case East:
		return South
	case South:
		return West
	case West:
		return North
	}
	return North
}

func (d Direction) TurnLeft() Direction {
	switch d {
	case North:
		return West
	case West:
		return South
	case South:
		return East
	case East:
		return North
	}
	return North
}

func (d Direction) Delta() (int, int) {
	switch d {
	case North:
		return 0, 1
	case East:
		return 1, 0
	case South:
		return 0, -1
	case West:
		return -1, 0
	}
	return 0, 0
}

type Instruction struct {
	Turn   rune
	Blocks int
}

func ParseInput(input string) []Instruction {
	parts := strings.Split(strings.TrimSpace(input), ", ")
	instructions := make([]Instruction, 0, len(parts))

	for _, part := range parts {
		if len(part) == 0 {
			continue
		}
		turn := rune(part[0])
		blocks, _ := strconv.Atoi(part[1:])
		instructions = append(instructions, Instruction{Turn: turn, Blocks: blocks})
	}

	return instructions
}

func Abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func SolvePart1(input string) int {
	instructions := ParseInput(input)

	x, y := 0, 0
	direction := North

	for _, instruction := range instructions {
		switch instruction.Turn {
		case 'R':
			direction = direction.TurnRight()
		case 'L':
			direction = direction.TurnLeft()
		}

		dx, dy := direction.Delta()
		x += dx * instruction.Blocks
		y += dy * instruction.Blocks
	}

	return Abs(x) + Abs(y)
}

func SolvePart2(input string) int {
	instructions := ParseInput(input)

	x, y := 0, 0
	direction := North
	visited := make(map[[2]int]bool)
	visited[[2]int{0, 0}] = true

	for _, instruction := range instructions {
		switch instruction.Turn {
		case 'R':
			direction = direction.TurnRight()
		case 'L':
			direction = direction.TurnLeft()
		}

		dx, dy := direction.Delta()

		for i := 0; i < instruction.Blocks; i++ {
			x += dx
			y += dy

			pos := [2]int{x, y}
			if visited[pos] {
				return Abs(x) + Abs(y)
			}
			visited[pos] = true
		}
	}

	return 0
}
