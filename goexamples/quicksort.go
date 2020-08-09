package goexamples

func Quicksort(x [6]int) [6]int {
	var l, r [15]int
	var n, d, li, re, i, j, p, pivot, tmp int
	l[0] = 0
	r[0] = 6 - 1

	d = 0
	n = 1
	for d != n {

		li = l[d]
		re = r[d]
		i = li
		j = re

		p = i
		p = p + j
		p = p >> 1
		pivot = x[p]

		for i <= j {
			for x[i] < pivot {
				i = i + 1
			}

			for x[j] > pivot {
				j = j - 1
			}

			if i <= j {
				if i < j {
					tmp = x[i]
					x[i] = x[j]
					x[j] = tmp
				}

				if i < 5 {
					i = i + 1
				}
				if j > 0 {
					j = j - 1
				}
			}
		}

		if li < j {
			l[n] = li
			r[n] = j
			n = n + 1
		}

		if i < re {
			l[n] = i
			r[n] = re
			n = n + 1
		}

		d = d + 1
	}

	return x
}
