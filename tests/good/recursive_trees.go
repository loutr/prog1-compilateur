package main
import "fmt"

type tree struct {
    is_empty bool; node int;
    ls *tree; rs *tree
}

func nodes(t *tree) int {
    if t.is_empty {
	return 0
    } else {
	return 1 + nodes(t.ls) + nodes(t.rs)
    }
}

func main() {
    a := new(tree)
    a.is_empty = true
    c := new(tree)
    c.is_empty, c.ls, c.rs, c.node = false, a, a, 5
    d := new(tree)
    d.is_empty, d.ls, d.rs, d.node = false, c, a, 3
    fmt.Print(nodes(d))
}
