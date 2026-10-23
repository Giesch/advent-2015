import "./input.txt" as puzzle_input : Str

main! : List(Str) => Try({}, _)
main! = |_args| {
	# TODO leaving off this ? causes a segfault
	# repro on latest main and file a bug
	part_one = solve_part_one(parsed_input)?
	echo!("Part One: ${part_one.to_str()}\n")

	Ok({})
}

Ok(parsed_input) =
	puzzle_input.trim().split_on("\n")
		.map_try(U64.from_str)
		.map_ok(List.sort_reversed)

expect Set.from_list(parsed_input).len() == parsed_input.len()
expect parsed_input.sum().is_multiple_of(3)

# FIXME this is still too slow
solve_part_one : List(U64) -> Try(U64, _)
solve_part_one = |weights| {
	group_weight = weights.sum() / 3

	# Start with the one option of keeping nothing
	var $first_variants = Set.single(Set.empty())
	var $remaining_max = weights.sum()

	for pkg in weights {
		$remaining_max = $remaining_max - pkg

		var $new_variants = $first_variants
		for existing_variant in $first_variants {
			existing_sum = existing_variant.iter().sum()
			potential_sum = existing_sum + pkg
			small_enough = potential_sum <= group_weight
			# large_enough = (potential_sum + $remaining_max) >= group_weight
			if small_enough { # and large_enough {
				$new_variants = $new_variants.insert(existing_variant.insert(pkg))
			}
		}

		# keep only the variants under or at the weight limit
		$first_variants = $new_variants
			.drop_if(|variant| variant.iter().sum() + $remaining_max < group_weight)
	}

	# keep only the variants that exactly match weight limit
	$first_variants = $first_variants.keep_if(|v| v.iter().sum() == group_weight)
	min_package_count = $first_variants.iter().map(Set.len).min() ? NoFirstGroup
	$first_variants = $first_variants.keep_if(|v| v.len() == min_package_count)

	SetWithProduct : { set : Set(U64), product : U64 }

	possible_first_groups : List(SetWithProduct)
	possible_first_groups = {
		with_product : Set(U64) -> Try(SetWithProduct, _)
		with_product = |set| {
			product = set.iter().product()?
			Ok({ set, product })
		}

		$first_variants.to_list()
			.keep_oks(with_product) # silently drop empties
			.sort_by(|item| item.product)
	}

	# # skip the rest to debug perf for the first group
	# # this succeeds fast enough in a release build
	# dbg possible_first_groups
	# Err(FirstPart(possible_first_groups))

	# now, find any solution for splitting the remaining into 2 equal groups
	# we only care if it exists; so try to find one way to sum to group_weight
	# and exit/continue otherwise
	for { set: first_group, product: entanglement } in possible_first_groups {
		expect first_group.iter().sum() == group_weight * 2

		# TODO iterate through all combinations of these,
		# looking for one that sums to group_weight
		_remaining_weights = weights
			.drop_if(|w| first_group.contains(w))

		can_sum_to_group_weight = False

		if can_sum_to_group_weight
			return Ok(entanglement)
	}

	Err(NoSolution)
}

powerset : List(item) -> Iter(Set(item))
	where [
		item.is_eq : item, item -> Bool,
		item.to_hash : item, Hasher -> Hasher,
	]
powerset = |items| {
	items.fold(
		Iter.single(Set.empty()),
		|subsets, item| {
			new_subsets = subsets.map(|subset| subset.insert(item))
			subsets.concat(new_subsets)
		},
	)
}

expect {
	items : List(U64)
	items = []

	powerset(items).collect() == [Set.empty()]
}

expect {
	subsets : List(Set(U64))
	subsets = powerset([1, 2, 3]).collect()

	subsets.len() == 8 and Set.from_list(subsets) == Set.from_list([
		Set.empty(),
		Set.single(1),
		Set.single(2),
		Set.single(3),
		Set.from_list([1, 2]),
		Set.from_list([1, 3]),
		Set.from_list([2, 3]),
		Set.from_list([1, 2, 3]),
	])
}

# check laziness
expect powerset((0.U64..<65).iter().collect()).take_first(2).collect()
	== [Set.empty(), Set.single(0)]
