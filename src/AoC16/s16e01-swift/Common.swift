import Foundation

// Common utilities for this day's solution

enum Direction: Int {
    case north = 0
    case east = 1
    case south = 2
    case west = 3

    func turnRight() -> Direction {
        return Direction(rawValue: (self.rawValue + 1) % 4)!
    }

    func turnLeft() -> Direction {
        return Direction(rawValue: (self.rawValue + 3) % 4)!
    }

    func delta() -> (x: Int, y: Int) {
        switch self {
        case .north:
            return (0, 1)
        case .east:
            return (1, 0)
        case .south:
            return (0, -1)
        case .west:
            return (-1, 0)
        }
    }
}

struct Instruction {
    let turn: Character
    let blocks: Int
}

func parseInput(_ input: String) -> [Instruction] {
    return input.trimmingCharacters(in: .whitespacesAndNewlines)
        .split(separator: ",")
        .map { s in
            let trimmed = s.trimmingCharacters(in: .whitespaces)
            let turn = trimmed.first!
            let blocks = Int(trimmed.dropFirst())!
            return Instruction(turn: turn, blocks: blocks)
        }
}

public func solvePart1(_ input: String) -> Int {
    let instructions = parseInput(input)

    var x = 0
    var y = 0
    var direction = Direction.north

    for instruction in instructions {
        if instruction.turn == "R" {
            direction = direction.turnRight()
        } else if instruction.turn == "L" {
            direction = direction.turnLeft()
        }

        let (dx, dy) = direction.delta()
        x += dx * instruction.blocks
        y += dy * instruction.blocks
    }

    return abs(x) + abs(y)
}

public func solvePart2(_ input: String) -> Int {
    let instructions = parseInput(input)

    var x = 0
    var y = 0
    var direction = Direction.north
    var visited = Set<String>()
    visited.insert("0,0")

    for instruction in instructions {
        if instruction.turn == "R" {
            direction = direction.turnRight()
        } else if instruction.turn == "L" {
            direction = direction.turnLeft()
        }

        let (dx, dy) = direction.delta()

        for _ in 0..<instruction.blocks {
            x += dx
            y += dy

            let key = "\(x),\(y)"
            if visited.contains(key) {
                return abs(x) + abs(y)
            }

            visited.insert(key)
        }
    }

    return 0
}
