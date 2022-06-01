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

	a = a
	t := a
	b = a

	if b != 0 {
		t = a
	}

	if b != 0 {
		x := 0
		b = 1
		a = 6
	}

	a = t

	return a + 1
}
