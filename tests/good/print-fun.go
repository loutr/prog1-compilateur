package main

import "fmt"

type L struct {
    x int
    y int
}

func foo() L {
    var l L

    l.x = 42
    l.y = 84

    return l
}

func main() {
    l := foo()

    fmt.Print(l, "\n")
}
