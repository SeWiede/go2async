package goexamples

// a = a + b*c
func MAC(a, b, c int) int {
	for b != 0 {
		t := b
		t = t & 1
		if t == 1 {
			a = a + c
		}
		c = c << 1
		b = b >> 1
	}
	return a
}
