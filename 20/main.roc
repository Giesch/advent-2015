# puzzle_input = 29_000_000.U64

main! : List(Str) => Try({}, _)
main! = |args| {
	input_str = args.get(0) ? MissingArg
	input = U64.from_str(input_str) ? NonNumberArg

	part_one = fourth(input)

	echo!("Part One: ${part_one.to_str()}\n")

	Ok({})
}

first : U64 -> U64
first = |input| {
	max_sum_of_factors = input / 10

	var $sums = List.repeat(0, max_sum_of_factors)
	var $current_best = max_sum_of_factors
	for elf in 1..<max_sum_of_factors {
		for house in elf..<$current_best {
			to_add = if house.is_multiple_of(elf) elf else 0
			sum = $sums.get(house).ok_or(0) + to_add
			$sums = $sums.set(house, sum).ok_or($sums)
			if sum >= max_sum_of_factors {
				$current_best = $current_best.min(house)
			}
		}
	}

	var $house = 0.U64
	while $house < $current_best {
		sum = $sums.get($house).ok_or(0)
		if sum >= max_sum_of_factors {
			break
		}

		$house = $house + 1
	}

	$house
}

second : U64 -> U64
second = |input| {
	max_sum_of_factors = input.div_by(10)

	var $factors_by_index = List.repeat(Set.empty(), max_sum_of_factors)
	for i in 1..<max_sum_of_factors {
		var $factors = Set.from_list([1, i])

		for candidate in 2..=i.div_by(2) {
			partner = i.div_by(candidate)
			product = candidate * partner
			if product == i {
				candidate_factors = match $factors_by_index.get(candidate) {
					Ok(n) => n
					Err(OutOfBounds) => crash "unreachable"
				}

				partner_factors = match $factors_by_index.get(partner) {
					Ok(n) => n
					Err(OutOfBounds) => crash "unreachable"
				}

				$factors = $factors.union(candidate_factors).union(partner_factors)
			}
		}

		sum = $factors.iter().sum()
		if sum >= max_sum_of_factors {
			return i
		}

		$factors_by_index = match $factors_by_index.set(i, $factors) {
			Ok(list) => list
			Err(OutOfBounds) => crash "unreachable"
		}
	}

	max_sum_of_factors
}

Entry : {
	# the number
	n : U64,
	# its factors/divisors
	factors : Set(U64),
	# sum of factors
	sum : U64,
	# we've found all factors below this number
	checked : U64,
}

empty_entry : Entry
empty_entry = { n: 0, factors: Set.empty(), sum: 0, checked: 0 }

multiply_entries : Entry, Entry -> Entry
multiply_entries = |left, right| {
	n = left.n * right.n
	factors = left.factors.union(right.factors)
	sum = factors.iter().sum()
	checked = left.n.max(right.n)

	{ n, factors, sum, checked }
}

third : U64 -> U64
third = |input| {
	max_sum_of_factors = input.div_by(10)
	var $factors_by_index = List.repeat(empty_entry, max_sum_of_factors.max(10))

	# FIXME factors can exist across chunks
	# need to search downwards in previous chunks or something
	var $first_chunk = True
	var $chunk_min = 0
	var $chunk_max = 10
	while $chunk_max < max_sum_of_factors or $first_chunk {
		$first_chunk = False
		var $chunk_res = Try.Err(None)

		for i in $chunk_min..<$chunk_max {
			for j in i..<$chunk_max {
				previous_entry : U64 -> Entry
				previous_entry = |index| {
					match $factors_by_index.get(index) {
						Ok(entry) => entry
						Err(OutOfBounds) => crash "previous_entry(${index.to_str()})"
					}
				}

				i_entry = previous_entry(i)
				j_entry = previous_entry(j)

				n = i * j
				if n < $factors_by_index.len() {
					n_entry = {
						original_n_entry = match $factors_by_index.get(n) {
							Ok(entry) => entry
							Err(OutOfBounds) => crash "future n entry: ${n.to_str()}"
						}

						factors = original_n_entry.factors.union(Set.from_list([1, n]))
						{ ..original_n_entry, factors }
					}

					factors = i_entry.factors.union(j_entry.factors).union(n_entry.factors)
					sum = factors.iter().sum()
					checked = i.min(j).max(n_entry.checked)
					new_entry = { n, factors, sum, checked }

					if sum >= max_sum_of_factors {
						res = $chunk_res.ok_or(U64.highest).min(n)
						if res > 0 {
							$chunk_res = Ok(res)
						}
					}

					$factors_by_index = match $factors_by_index.set(n, new_entry) {
						Ok(arr) => arr
						Err(OutOfBounds) => crash "$factors_by_index.set(${n.to_str()}, _)"
					}
				}
			}
		}

		match $chunk_res {
			Ok(res) => return res
			Err(None) => {}
		}

		$chunk_min = $chunk_min + 10
		$chunk_max = $chunk_max + 10
	}

	max_sum_of_factors
}

fourth : U64 -> U64
fourth = |input| {
	max_sum_of_factors = input / 10

	lower_bound : U64
	lower_bound = {
		list = List.from_iter(
			harmonic_series
				.with_index()
				.drop_if(|(_index, sum)| sum < max_sum_of_factors)
				.map(|(index, _sum)| index)
				.take_first(1),
		)

		match list.first() {
			Ok(sum) => sum
			_ => crash "oops"
		}
	}

	var $sums = List.repeat(0, max_sum_of_factors)
	var $current_best = max_sum_of_factors
	for elf in 1..<max_sum_of_factors {
		if elf > $current_best break

		# first multiple past the lower bound
		var $house = ((lower_bound + elf - 1) / elf) * elf

		while $house < $current_best {
			sum = $sums.get($house).ok_or(0) + elf
			$sums = match $sums.set($house, sum) {
				Ok(sums) => sums
				Err(OutOfBounds) => crash "oops"
			}

			if sum >= max_sum_of_factors {
				$current_best = $current_best.min($house)
			}

			$house = $house + elf
		}
	}

	var $house = 0.U64
	while $house < $current_best {
		sum = $sums.get($house).ok_or(0)
		if sum >= max_sum_of_factors {
			break
		}

		$house = $house + 1
	}

	$house
}

harmonic_series = {
	start = { sum: 0.U64, n: 0.U64 }

	advance = |state| {
		n = state.n + 1
		sum = state.sum + state.n

		Ok((sum, { sum, n }))
	}

	Iter.custom(start, Unknown, advance)
}

expect harmonic_series.take_first(5).collect() == [0, 1, 3, 6, 10]

expect first(10) == 1
expect first(70) == 4
expect first(50) == 4
expect first(120) == 6

expect second(10) == 1
expect second(70) == 4
expect second(50) == 4
expect second(120) == 6

expect third(10) == 1
expect third(70) == 4
expect third(50) == 4
expect third(120) == 6

expect fourth(10) == 1
expect fourth(70) == 4
expect fourth(50) == 4
expect fourth(120) == 6
