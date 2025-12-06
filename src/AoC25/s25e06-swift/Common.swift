import Foundation

public func parseInput(_ input: String) -> [String] {
    input.components(separatedBy: .newlines)
}

public func solvePart1(_ input: String) -> UInt64 {
    var lines = parseInput(input)
    let operations = lines.removeLast().split(separator: " ")

    let data = lines.map {
        $0.split(separator: " ").map { UInt64($0)! }
    }

    return operations.enumerated().map { index, operation in
        switch operation {
        case "+": return data.map { $0[index] }.reduce(0, +)
        case "*": return data.map { $0[index] }.reduce(1, *)
        default: fatalError("Unsupported operation \(operation)")
        }
    }.reduce(0, +)
}

public func solvePart2(_ input: String) -> UInt64 {
    let lines = padLines(parseInput(input).map { Array($0) })
    let spaces = identifyColumnSeparators(lines)
    let (rows, operations) = extractData(lines, separatedBy: spaces)
    let numbers = translateMath(rows)
    return compute(numbers, operations: operations).reduce(0, +)
}

func padLines(_ lines: [[Character]]) -> [[Character]] {
    let maxLength = lines.map(\.count).max()!
    return lines.map {
        $0 + Array(repeating: " ", count: max(0, maxLength - $0.count))
    }
}

func identifyColumnSeparators(_ lines: [[Character]]) -> [Int] {
    let spaceSets = lines.map { line in
        Set(line.indices.filter { line[$0] == " " })
    }

    return spaceSets
        .reduce(spaceSets.first!) { $0.intersection($1) }
        .sorted()
}

func extractData(_ lines: [[Character]], separatedBy separators: [Int]) -> ([[[Character]]], [String]) {
    var rows = extractCells(lines, separatedBy: separators)
    let operations_row = rows.removeLast()
    let operations = extractOperations(operations_row)
    return (rows, operations)
}

func extractCells(_ lines: [[Character]], separatedBy separators: [Int]) -> [[[Character]]] {
    lines.map { characters in
        let starts = [0] + separators.map { $0 + 1 }
        let ends = separators + [characters.count]
        return zip(starts, ends).map { start, end in
            Array(characters[start ..< end])
        }
    }
}

func extractOperations(_ ops_line: [[Character]]) -> [String] {
    ops_line.map {
        String($0).trimmingCharacters(in: .whitespaces)
    }
}

func translateMath(_ rows: [[[Character]]]) -> [[UInt64]] {
    transpose(rows).map { column in
        transpose(column).map { cell in
            UInt64(String(cell).trimmingCharacters(in: .whitespaces))!
        }
    }
}

func transpose<T>(_ matrix: [[T]]) -> [[T]] {
    matrix.first!.indices.map { index in
        matrix.map { $0[index] }
    }
}

func compute(_ numbers: [[UInt64]], operations: [String]) -> [UInt64] {
    zip(operations, numbers).map { operation, values in
        switch operation {
        case "+": return values.reduce(0, +)
        case "*": return values.reduce(1, *)
        default: fatalError("Unsupported operation \(operation)")
        }
    }
}
