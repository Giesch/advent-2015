import "./input.txt" as puzzle_input : Str

Ok(parsed_nums) = parse_lines(puzzle_input)

main! = |_args| {
	Ok(buckets) = to_buckets(parsed_nums)
	{ part_one, part_two } = solve(buckets)

	# { part_one, part_two } = dynamic_programming_solve(nums)

	echo!("Part One Total: ${part_one.to_str()}\n")
	echo!("Part Two Count: ${part_two.to_str()}\n")

	Ok({})
}

solve : Buckets -> { part_one : U64, part_two : U64 }
solve = |buckets| {
	# Lane i of weights holds the single bit 1 << (i % 8).
	powers_of_two = [1, 2, 4, 8, 16, 32, 64, 128]
	Ok(weights) = U8x16.from_list(powers_of_two.concat(powers_of_two))
	Ok(upper_half) = U8x16.from_list(List.repeat(0, 8).concat(List.repeat(255, 8)))

	var $part_one_total = 0.U64
	var $min_containers = U64.highest
	var $part_two_min_counts = Dict.empty()
	n_attempts = (2.U64).pow(buckets.len)
	for attempt in 0..<n_attempts {
		# `attempt` is < 2^20, so we break it up into 3 bytes
		b0 = attempt.to_u8_wrap()
		b1 = attempt.shr_wrap(8).to_u8_wrap()
		b2 = attempt.shr_wrap(16).to_u8_wrap()

		# Broadcast the low bytes of attempt: lanes 0..7 hold b0, lanes 8..15 hold b1.
		bytes_lo = upper_half.bit_select(U8x16.splat(b1), U8x16.splat(b0))
		# The `and` isolates one attempt bit per lane.
		# eq_lanes widens each set bit into an all-ones lane,
		# so lane i is 255 when bit i of attempt is set.
		lo_mask = bytes_lo.bitwise_and(weights).eq_lanes(weights)
		# Lanes 8..15 retest bits 16..23, but buckets.hi is padded zero there.
		hi_mask = U8x16.splat(b2).bitwise_and(weights).eq_lanes(weights)

		# Keep a capacity where the mask lane is all ones; put zero elsewhere.
		masked_lo = lo_mask.bit_select(buckets.lo, U8x16.splat(0))
		masked_hi = hi_mask.bit_select(buckets.hi, U8x16.splat(0))

		lo_sum = masked_lo.sum_lanes()
		hi_sum = masked_hi.sum_lanes()
		sum = lo_sum + hi_sum

		if sum == 150 {
			$part_one_total = $part_one_total + 1
			containers = attempt.count_one_bits().to_u64()
			$min_containers = $min_containers.min(containers)
			if containers == $min_containers {
				$part_two_min_counts = $part_two_min_counts.update(
					containers,
					|entry| Ok(entry.ok_or(0) + 1),
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
