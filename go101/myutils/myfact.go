package utils

func Fact(n uint) uint {
	if (n < 1) {
		return 1
	} else {
		return n * Fact(n-1)
	}
}

