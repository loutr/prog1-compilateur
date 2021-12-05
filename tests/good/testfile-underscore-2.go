package main
import "fmt"

func g() (int, int, int) {
    return 1, 2, 3
}

func main() {
	var _, x, _ = g()
	fmt.Print(x)
}
