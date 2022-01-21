package main
import "fmt"

func fact(n int) int {
    r := 1;
    for ; n > 1; n-- {
	r = r * n;
    }
    return r;
}

func main() {
    fmt.Print(1 > 1, "\n");
    for n := 0; n <= 10; n++ {
	if n > 4 {
	    fmt.Print(n, "! = ", fact(n), "\n");
	} else {
	    fmt.Print(n, "?\n");
	}
    }
}
