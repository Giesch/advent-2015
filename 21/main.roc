import "./input.txt" as puzzle_input : Str

Ok(boss_stats) = parse_boss_stats(puzzle_input)

player_hp = 100

Ok(part_one) = all_purchase_options
	.map(Loadout.from_list)
	.keep_if(|l| l.can_win())
	.sort_by(|l| l.cost())
	.first()

Ok(part_two) = all_purchase_options
	.map(Loadout.from_list)
	.keep_if(|l| !l.can_win())
	.sort_by(|l| l.cost())
	.last()

main! : List(Str) => Try({}, _)
main! = |_args| {
	echo!("Part One: ${part_one.cost().to_str()}\n")
	echo!("Part Two: ${part_two.cost().to_str()}\n")

	Ok({})
}

BossStats : {
	hp : U64,
	damage : U64,
	armor : U64,
}

parse_boss_stats : Str -> Try(BossStats, _)
parse_boss_stats = |input| {
	lines = input.trim().split_on("\n")

	read_stat_line = |i| {
		line = lines.get(i)?
		str = line.split_on(": ").get(1)?
		U64.from_str(str)
	}

	hp = read_stat_line(0)?
	damage = read_stat_line(1)?
	armor = read_stat_line(2)?

	Ok({ hp, damage, armor })
}

Item : {
	name : Str,
	cost : U64,
	damage : U64,
	armor : U64,
}

Shop := {
	name : Str,
	required_purchases : List(U64),
	items : List(Item),
}.{
	possible_purchases : Shop -> List(List(Item))
	possible_purchases = |Shop.({ .., required_purchases, items })| {
		zero_options =
			if required_purchases.contains(0)
				[[]]
			else
				[]

		one_options =
			if required_purchases.contains(1)
				items.map(|item| [item])
			else
				[]

		two_options =
			if required_purchases.contains(2) {
				get_unwrap_item = |i| match items.get(i) {
					Ok(item) => item
					Err(OutOfBounds) => crash "unreachable"
				}

				var $options = []
				for i in 0..<items.len() {
					for j in i..<items.len() {
						first_item = get_unwrap_item(i)
						second_item = get_unwrap_item(j)
						$options = $options.append([first_item, second_item])
					}
				}

				$options
			} else {
				[]
			}

		zero_options
			.concat(one_options)
			.concat(two_options)
	}
}

shops : List(Shop)
shops = [
	{
		name: "Weapons",
		required_purchases: [1],
		items: all_weapons,
	},
	{
		name: "Armor",
		required_purchases: [0, 1],
		items: all_armors,
	},
	{
		name: "Rings",
		required_purchases: [0, 1, 2],
		items: all_rings,
	},
]

all_weapons : List(Item)
all_weapons =
	[
		{ name: "Dagger", cost: 8, damage: 4 },
		{ name: "Shortsword", cost: 10, damage: 5 },
		{ name: "Warhammer", cost: 25, damage: 6 },
		{ name: "Longsword", cost: 40, damage: 7 },
		{ name: "Greataxe", cost: 74, damage: 8 },
	]
		.map(|{ name, cost, damage }| { name, cost, damage, armor: 0 })

all_armors : List(Item)
all_armors =
	[
		{ name: "Leather", cost: 13, armor: 1 },
		{ name: "Chainmail", cost: 31, armor: 2 },
		{ name: "Splintmail", cost: 53, armor: 3 },
		{ name: "Bandedmail", cost: 75, armor: 4 },
		{ name: "Platemail", cost: 102, armor: 5 },
	]
		.map(|{ name, cost, armor }| { name, cost, damage: 0, armor })

all_rings : List(Item)
all_rings = [
	{ name: "Damage +1", cost: 25, damage: 1, armor: 0 },
	{ name: "Damage +2", cost: 50, damage: 2, armor: 0 },
	{ name: "Damage +3", cost: 100, damage: 3, armor: 0 },
	{ name: "Armor +1", cost: 20, damage: 0, armor: 1 },
	{ name: "Armor +2", cost: 40, damage: 0, armor: 2 },
	{ name: "Armor +3", cost: 80, damage: 0, armor: 3 },
]

Loadout := List(Item).{
	is_eq : _

	from_list : List(Item) -> Loadout
	from_list = |l| Loadout.(l)

	turns_to_kill : Loadout -> Try(U64, [Never])
	turns_to_kill = |Loadout.(items)| {
		if items.is_empty() return Err(Never)

		damage = items.map(|item| item.damage).iter().sum()
		if boss_stats.armor >= damage return Err(Never)

		effective_damage = damage - boss_stats.armor
		ttk = boss_stats.hp.div_ceil_by(effective_damage)

		Ok(ttk)
	}

	turns_to_die : Loadout -> Try(U64, [Never])
	turns_to_die = |Loadout.(items)| {
		armor = items.map(|item| item.armor).iter().sum()
		if boss_stats.damage <= armor return Err(Never)

		effective_damage = boss_stats.damage - armor
		ttd = player_hp.div_ceil_by(effective_damage)

		Ok(ttd)
	}

	can_win : Loadout -> Bool
	can_win = |loadout|
		match (loadout.turns_to_kill(), loadout.turns_to_die()) {
			(Ok(ttk), Ok(ttd)) => ttk <= ttd
			(Ok(_ttk), Err(Never)) => True
			(Err(Never), Ok(_ttd)) => False
			(Err(Never), Err(Never)) => False
		}

	cost : Loadout -> U64
	cost = |Loadout.(items)|
		items.iter().map(|item| item.cost).sum()
}

all_purchase_options : List(List(Item))
all_purchase_options = {
	# start with one option; the option of buying nothing
	var $all_options = [[]]

	# visit each shop, adding all the possible purchases to each existing option
	for shop in shops {
		var $purchases = []

		for new_purchases in shop.possible_purchases() {
			for existing_purchases in $all_options {
				purchase = existing_purchases.concat(new_purchases)
				$purchases = $purchases.append(purchase)
			}
		}

		$all_options = $purchases
	}

	$all_options
}
