package main

import (
	"fmt"
	"io"
	"os"

	"template-go/common"
)

func main() {
	data, _ := io.ReadAll(os.Stdin)
	result := common.SolvePart1(string(data))
	fmt.Println(result)
}
