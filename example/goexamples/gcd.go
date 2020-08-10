package goexamples

func GCD(a, b int) int {
	for b != 0 {
		t := b
		//b = a % b
		c := a
		for c >= b {
			c = c - b
		}
		b = c
		a = t
	}
	return a
}
