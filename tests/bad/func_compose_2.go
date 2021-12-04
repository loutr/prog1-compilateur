package main

func bar(x int, y int) int {
	return x + y
}

func main() {
	x := bar(bar(8))
}
