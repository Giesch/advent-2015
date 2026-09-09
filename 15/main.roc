import "./input.txt" as puzzle_input : Str

## The puzzle input, converted into two SIMD vectors
Buckets : { hi : U8x16, lo : U8x16, len : U64 }

Ok(parsed_nums) = parse_lines(puzzle_input)

main! : List(Str) => Try({}, _)
main! = |_args| {
	Ok(buckets) = to_buckets(parsed_nums)
	{ part_one, part_two } = solve(buckets)

	echo!("Part One Total: ${part_one.to_str()}\n")
	echo!("Part Two Count: ${part_two.to_str()}\n")

	Ok({})
}

# the set of single-bit selectors for a byte
# this lets a SIMD lane represent a single input byte from the attempt mask
powers_of_two = [1, 2, 4, 8, 16, 32, 64, 128]

Ok(weights) = U8x16.from_list(powers_of_two.concat(powers_of_two))

# a selector for the top half, more significant bits
# (the input list is little-endian; 255 means 'keep this lane/these bits')
Ok(upper_half) = U8x16.from_list(List.repeat(0, 8).concat(List.repeat(255, 8)))

solve : Buckets -> { part_one : U64, part_two : U64 }
solve = |buckets| {
	var $ways_to_150 = 0.U64
	var $min_containers_used = U64.highest
	var $num_best_ways = 0

	# convert every number from 0 to 2^20 to a bitmask selecting containers,
	# and sum and the results of each
	n_attempts = (2.U64).pow(buckets.len)
	for attempt in 0..<n_attempts {
		# `attempt` is < 2^20, so we can break it up into only 3 bytes
		b0 = attempt.to_u8_wrap() # the rightmost least significant byte
		b1 = attempt.shr_wrap(8).to_u8_wrap() # the middle byte
		b2 = attempt.shr_wrap(16).to_u8_wrap() # the left, most significant byte

		# Broadcast the low bytes of attempt: lanes 0..7 hold b0, lanes 8..15 hold b1.
		bytes_lo = upper_half.bit_select(U8x16.splat(b1), U8x16.splat(b0))
		# Lanes 8..15 retest bits 16..23, but buckets.hi is padded zero there.
		bytes_hi = U8x16.splat(b2)

		# The `and` isolates one attempt bit per lane.
		# U8x16.eq_lanes widens each set bit into an all-ones lane,
		# so lane i is 255 when bit i of attempt is set.
		lo_mask = bytes_lo.bitwise_and(weights).eq_lanes(weights)
		hi_mask = bytes_hi.bitwise_and(weights).eq_lanes(weights)

		# Keep a capacity where the mask lane is all ones; put zero elsewhere.
		lo_kept_bucket_values = lo_mask.bit_select(buckets.lo, U8x16.splat(0))
		hi_kept_bucket_values = hi_mask.bit_select(buckets.hi, U8x16.splat(0))

		sum = lo_kept_bucket_values.sum_lanes() + hi_kept_bucket_values.sum_lanes()

		if sum == 150 {
			$ways_to_150 = $ways_to_150 + 1
			containers_used = attempt.count_one_bits().to_u64()
			if containers_used < $min_containers_used {
				$min_containers_used = containers_used
				$num_best_ways = 0
			}
			if containers_used == $min_containers_used {
				$num_best_ways = $num_best_ways + 1
			}
		}
	}

	{ part_one: $ways_to_150, part_two: $num_best_ways }
}

parse_lines : Str -> Try(List(U8), _)
parse_lines = |input|
	input
		.trim()
		.split_on("\n")
		.map_try(|line| U8.from_str(line))

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
