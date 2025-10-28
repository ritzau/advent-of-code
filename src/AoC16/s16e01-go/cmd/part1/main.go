package main

import (
	"fmt"
	"io"
	"os"

	"s16e01/common"
)

func main() {
	data, _ := io.ReadAll(os.Stdin)
	result := common.SolvePart1(string(data))
	fmt.Println(result)
}
