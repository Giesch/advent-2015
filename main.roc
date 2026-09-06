import "./input.txt" as puzzle_input : Str

main! = |_args| {
	Ok(nums) = parse_lines(puzzle_input)
	Ok(buckets) = to_buckets(nums)

	{ part_one, part_two } = solve(buckets)

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
		Err(_) => crash "unreachable"
	}

	{ part_one: $part_one_total, part_two: part_two_count }
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

	Try.Ok({ hi, lo, len })
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
