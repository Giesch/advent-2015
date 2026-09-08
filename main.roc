import "./input.txt" as puzzle_input : Str

main! = |_args| {
	Ok(nums) = parse_lines(puzzle_input)

	# Ok(buckets) = to_buckets(nums)
	# { part_one, part_two } = solve(buckets)

	{ part_one, part_two } = dynamic_programming_solve(nums)

	echo!("Part One Total: ${part_one.to_str()}\n")
	echo!("Part Two Count: ${part_two.to_str()}\n")

	Ok({})
}

solve : Buckets -> { part_one : U64, part_two : U64 }
solve = |buckets| {
	n_attempts = (2.U64).pow(buckets.len)
	var $part_one_total = 0.U64
	var $min_containers = U64.highest
	var $part_two_min_counts = Dict.empty()
	for attempt in 0..<n_attempts {
		var $lo_mask = U8x16.default()
		var $lo_containers = 0
		for bit in 0..<16 {
			should_include_bucket = attempt.shr_wrap(bit).bitwise_and(1) == 1
			if should_include_bucket {
				$lo_containers = $lo_containers + 1
			}
			include_lane = if should_include_bucket U8.highest else 0
			$lo_mask = $lo_mask.with_lane(bit.to_u64(), include_lane)
		}

		var $hi_mask = U8x16.default()
		var $hi_containers = 0
		for bit in 0..<16 {
			should_include_bucket = attempt.shr_wrap(bit + 16).bitwise_and(1) == 1
			if should_include_bucket {
				$hi_containers = $hi_containers + 1
			}
			include_lane = if should_include_bucket U8.highest else 0
			$hi_mask = $hi_mask.with_lane(bit.to_u64(), include_lane)
		}

		masked_lo = $lo_mask.bit_select(buckets.lo, U8x16.default())
		masked_hi = $hi_mask.bit_select(buckets.hi, U8x16.default())

		lo_sum = masked_lo.sum_lanes()
		hi_sum = masked_hi.sum_lanes()
		sum = lo_sum + hi_sum

		if sum == 150 {
			$part_one_total = $part_one_total + 1
			containers = $hi_containers + $lo_containers
			if containers < $min_containers {
				$min_containers = containers
			}
			if containers == $min_containers {
				$part_two_min_counts = $part_two_min_counts.update(
					containers,
					|entry| match entry {
						Ok(found) => Ok(found + 1)
						Err(Missing) => Ok(1.U64)
					},
				)
			}
		}
	}

	part_two_count = match $part_two_min_counts.get($min_containers) {
		Ok(min) => min
		Err(_) => crash "unreachable: $min_containers always gets inserted above"
	}

	{
		part_one: $part_one_total,
		part_two: part_two_count,
	}
}

dynamic_programming_solve : List(U8) -> { part_one : U64, part_two : U64 }
dynamic_programming_solve = |nums| {
	target = 150.U64

	unreachable_slot = { ways: 0.U64, best: U64.highest, best_ways: 0.U64 }
	first_slot = { ways: 1.U64, best: 0.U64, best_ways: 1.U64 }
	Ok(initial) = List.repeat(unreachable_slot, target + 1).set(0, first_slot)

	var $states = initial
	for num in nums {
		capacity = num.to_u64()
		# Descending sums ensure each container is used at most once.
		var $next_sum = target + 1
		while $next_sum > capacity {
			$next_sum = $next_sum - 1
			sum = $next_sum
			source = match $states.get(sum - capacity) {
				Ok(state) => state
				Err(OutOfBounds) => crash "source volume must be between zero and target"
			}

			if source.ways != 0 {
				current = match $states.get(sum) {
					Ok(state) => state
					Err(OutOfBounds) => crash "destination volume must be between zero and target"
				}
				candidate = source.best + 1
				{ best, best_ways } = if candidate < current.best {
					{ best: candidate, best_ways: source.best_ways }
				} else if candidate == current.best {
					bw = current.best_ways + source.best_ways
					{ best: current.best, best_ways: bw }
				} else {
					{ best: current.best, best_ways: current.best_ways }
				}

				ways = current.ways + source.ways
				$states = match $states.set(sum, { ways, best, best_ways }) {
					Ok(updated) => updated
					Err(OutOfBounds) => crash "destination volume must be between zero and target"
				}
			}
		}
	}

	result = match $states.get(target) {
		Ok(state) => state
		Err(_) => crash "state table must include the target volume"
	}

	{
		part_one: result.ways,
		part_two: result.best_ways,
	}
}

parse_lines : Str -> Try(List(U8), _)
parse_lines = |input|
	input
		.trim()
		.split_on("\n")
		.map_try(|line| U8.from_str(line))

Buckets : { hi : U8x16, lo : U8x16, len : U64 }

to_buckets : List(U8) -> Try(Buckets, _)
to_buckets = |nums| {
	len = nums.len()
	expect len == 20

	aligned_bytes = nums.concat(List.repeat(0, 32 - len))

	lo = U8x16.load(aligned_bytes, 0)?
	hi = U8x16.load(aligned_bytes, 16)?

	Ok({ hi, lo, len })
}

expect {
	nums = parse_lines(puzzle_input)?
	buckets = to_buckets(nums)?
	solve(buckets).part_one == 4372
}

expect {
	nums = parse_lines(puzzle_input)?
	buckets = to_buckets(nums)?
	solve(buckets).part_two == 4
}

expect {
	nums = parse_lines(puzzle_input)?
	dynamic_programming_solve(nums).part_one == 4372
}

expect {
	nums = parse_lines(puzzle_input)?
	dynamic_programming_solve(nums).part_two == 4
}

# Equal capacities are distinct containers; only the one-container solution is minimal.
expect dynamic_programming_solve([150, 75, 75, 75]) == { part_one: 4, part_two: 1 }

# A single container cannot be reused to reach the target.
expect dynamic_programming_solve([75]) == { part_one: 0, part_two: 0 }

expect dynamic_programming_solve([75, 75, 75]) == { part_one: 3, part_two: 3 }
