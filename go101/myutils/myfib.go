package utils

import "fmt"

func Fib(n uint) uint {
	fmt.Printf("Fib entrypoint\n")
	return fib(n)
}

func fib(n uint) uint {
	if (n < 2) {
		return n
	} else {
		return fib(n-1) + fib(n-2)
	}
}

