package main

func foo(x int) (int, int) {
	return x, x+1
}

func bar(x int, y int) int {
	return x + y
}

func main() {
	x := bar(foo(3, 8))
}
