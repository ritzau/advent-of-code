package main

import (
	"fmt"
	"io"
	"os"

	"s16e01/common"
)

func main() {
	data, _ := io.ReadAll(os.Stdin)
	result := common.SolvePart2(string(data))
	fmt.Println(result)
}
