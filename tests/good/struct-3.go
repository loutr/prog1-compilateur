package main

import "fmt"

type M struct {
    mx int
    my N
    mz string
}

type N struct {
    ny string
}

func main() {
    var m M

    m.mx = 4
    m.my.ny = "Hello"
    m.mz = "World"

    fmt.Print(m, "\n");
}
