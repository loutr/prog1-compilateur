package main
import "fmt"

type A struct {
    a int
    b int
}

func main() {
    x, y := 0, 1
    x, y = y, x
    fmt.Print(x, y, "\n")
    i, j := new(A), new(A)
    i.a = 5
    j.a, j.b = 4, 8
    i, j = j, i
    fmt.Print(i, j, "\n")
}
