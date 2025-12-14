package main

import (
	"fmt"
	"io"
	"os"

	"s25e12-go/common"
)

func main() {
	data, _ := io.ReadAll(os.Stdin)
	result := common.SolvePart1(string(data))
	fmt.Println(result)
}
