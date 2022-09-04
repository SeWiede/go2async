package goexamples

func Quicksort(x [6]int) [6]int {
	var l, r [15]int // Queues to safe the recursive states (left bound, right bound)
	var n, d int     // Queue states
	l[0] = 0         // first left bound: leftmost element
	r[0] = 6 - 1     // first right bound: rightmost element

	d = 0        // current queue position
	n = 1        // occupied queue elements
	for d != n { // loop until all queue elements are processes
		li := l[d]
		re := r[d]
		i := li
		j := re

		p := j // pivot position

		// partitioning
		for i <= j {
			{ // Block to determine the positions i,j of elements to swap
				pivot := x[p]

				for x[i] < pivot {
					i = i + 1
				}

				for x[j] > pivot {
					j = j - 1
				}
			} // End of block: pivot variable is not visible outside this block

			if i <= j {
				if i < j {
					tmp := x[i]
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

		// determine left and right bounds for further partionings
		if li < j {
			l[n] = li
			r[n] = j
			n = n + 1 // increment queue element number
		}

		if i < re {
			l[n] = i
			r[n] = re
			n = n + 1
		}

		d = d + 1 // increment queue position
	}

	return x
}
