package goexamples

// a = a + b*c
func MAC(a int8, b int, c int8) int8 {
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
