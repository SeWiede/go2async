package goexamples

func TEST(a int8, b int, c int8) int8 {
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

/*
i := 0
	i := 0
	x := 0
	for i < 5 {
		j := i
		j = j + 1
		for j < 6 {
			if a[i] > a[j] {
				x = a[i]
				a[i] = a[j]
				a[j] = x
			}
			j = j + 1
		}
		i = i + 1
	}
	return a
*/
