import Foundation

func main() {
    var input = ""
    while let line = readLine() {
        input += line + "\n"
    }
    input = input.trimmingCharacters(in: .whitespacesAndNewlines)
    let result = solvePart2(input)
    print(result)
}

main()
