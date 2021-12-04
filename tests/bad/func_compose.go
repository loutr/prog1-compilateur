package main

func foo(x int) (int, int) {
	return x, x+1
}

func bar(x int, y int) int {return x + y}

func main() {
	x := foo(foo(8))
}
