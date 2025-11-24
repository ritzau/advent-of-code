import Foundation

func main() {
    var input = ""
    while let line = readLine() {
        input += line + "\n"
    }
    input = input.trimmingCharacters(in: .whitespacesAndNewlines)
    let result = solvePart1(input)
    print(result)
}

main()
