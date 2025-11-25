package main

import "fmt"
import "top.exe/myutils"

func main() {
	fmt.Printf("*go101*\n")
	mysub()
	test(10)
	test(11)
}

func test(arg uint) {
	fmt.Printf("fib(%d) -> %d\n",arg,utils.Fib(arg))
	fmt.Printf("fact(%d) -> %d\n",arg,utils.Fact(arg))
}
