package main
import "fmt"

type A struct {
    i int;
    s string;
    p *A;
}

func main() {
    var a A;
    b := new(A)
    a.i = 5
    a.s = "bonjour"
    a.p = &a
    fmt.Print(a, "\n");
    fmt.Print(b, "\n");
    fmt.Print(a.p, "\n");
}
