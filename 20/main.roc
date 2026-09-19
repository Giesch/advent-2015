

main! : List(Str) => Try({}, _)
main! = |args| {
	input_str = args.get(0) ? MissingArg
	input = U64.from_str(input_str) ? NonNumberArg

	part_one = solve_part_one(input)
	part_two = solve_part_two(input)

	echo!("Part One: ${part_one.to_str()}\n")
	echo!("Part Two: ${part_two.to_str()}\n")

	Ok({})
}

solve_part_one : U64 -> U64
solve_part_one = |input| {
	presents_multiple = 10
	delivery_limit = Err(None)

	solve({ input, presents_multiple, delivery_limit })
}

solve_part_two : U64 -> U64
solve_part_two = |input| {
	presents_multiple = 11
	delivery_limit = Ok(50)

	solve({ input, presents_multiple, delivery_limit })
}

InputConfig : {
	input : U64,
	presents_multiple : U64,
	delivery_limit : Try(U64, [None]),
}

solve : InputConfig -> U64
solve = |{ input, presents_multiple, delivery_limit }| {
	max_sum_of_factors = input / presents_multiple

	lower_bound : U64
	lower_bound = {
		first_found = harmonic_series
			.with_index()
			.drop_if(|(_index, sum)| sum < max_sum_of_factors)
			.map(|(index, _sum)| index)
			|> find_first

		match first_found {
			Ok(bound) => bound
			_ => crash "unreachable"
		}
	}

	var $sums = List.repeat(0, max_sum_of_factors)
	var $current_best = max_sum_of_factors
	for elf in 1..<max_sum_of_factors {
		if elf > $current_best
			break

		# first multiple past the lower bound
		var $house = ((lower_bound + elf - 1) / elf) * elf
		var $delivered = 0

		while $house < $current_best {
			sum = $sums.get($house).ok_or(0) + elf
			$sums = match $sums.set($house, sum) {
				Ok(sums) => sums
				Err(OutOfBounds) => crash "oops"
			}

			$delivered = $delivered + 1
			limit_reached = delivery_limit.map_ok(|l| $delivered >= l).ok_or(False)
			if limit_reached
				break

			if sum >= max_sum_of_factors {
				$current_best = $current_best.min($house)
			}

			$house = $house + elf
		}
	}

	var $house = 0.U64
	while $house < $current_best {
		sum = $sums.get($house).ok_or(0)
		if sum >= max_sum_of_factors
			break

		$house = $house + 1
	}

	$house
}

harmonic_series : Iter(U64)
harmonic_series = {
	start = { sum: 0.U64, n: 0.U64 }

	advance = |state| {
		n = state.n + 1
		sum = state.sum + state.n

		Ok((sum, { sum, n }))
	}

	Iter.custom(start, Unknown, advance)
}

expect harmonic_series.take_first(5).collect()
	== [0, 1, 3, 6, 10]

find_first : Iter(item) -> Try(item, [IterWasEmpty])
find_first = |iterator|
	match Iter.next(iterator) {
		Done => Err(IterWasEmpty)
		One({ item, .. }) => Ok(item)
		Skip({ rest }) => find_first(rest)
	}

expect {
	five_to_nine = (0..<10)
		.iter()
		.drop_if(|n| n < 5)

	find_first(five_to_nine) == Ok(5)
}

expect solve_part_one(10) == 1
expect solve_part_one(70) == 4
expect solve_part_one(50) == 4
expect solve_part_one(120) == 6

puzzle_input = 29_000_000.U64
expect solve_part_one(puzzle_input) == 665280
expect solve_part_two(puzzle_input) == 705600
