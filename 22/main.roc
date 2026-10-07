import "./input.txt" as puzzle_input : Str

player_hp = 50

player_mana = 500

part_one = solve({ hard_mode: False })

part_two = solve({ hard_mode: True })

main! : List(Str) => Try({}, _)
main! = |_args| {
	echo!("Part One: ${part_one.to_str()}\n")
	echo!("Part Two: ${part_two.to_str()}\n")

	Ok({})
}

parse_boss_stats : Str -> Try(Boss, _)
parse_boss_stats = |input| {
	lines = input.trim().split_on("\n")

	read_stat_line = |i| {
		line = lines.get(i)?
		str = line.split_on(": ").get(1)?
		U64.from_str(str)
	}

	raw_hp = read_stat_line(0)?
	hp = raw_hp.to_i64_wrap()

	damage = read_stat_line(1)?

	Ok({ hp, damage })
}

Spell := [
	MagicMissle,
	Drain,
	Shield,
	Poison,
	Recharge,
].{
	is_eq : _

	all : List(Spell)
	all = [
		MagicMissle,
		Drain,
		Shield,
		Poison,
		Recharge,
	]

	cost : Spell -> U64
	cost = |spell| match spell {
		MagicMissle => 53
		Drain => 73
		Shield => 113
		Poison => 173
		Recharge => 229
	}

	damage : Spell -> Try(U64, [None])
	damage = |spell| match spell {
		MagicMissle => Ok(4)
		Drain => Ok(2)
		_ => Err(None)
	}

	heal : Spell -> Try(U64, [None])
	heal = |spell| match spell {
		Drain => Ok(2)
		_ => Err(None)
	}

	effect : Spell -> Try(Buff, [None])
	effect = |spell| match spell {
		Shield => Ok(Shield)
		Poison => Ok(Poison)
		Recharge => Ok(Recharge)
		_ => Err(None)
	}
}

Buff : [Shield, Poison, Recharge]

Battle := { player : Player, boss : Boss, hard_mode : Bool }.{
	cast_spell : Battle, Spell -> Battle
	cast_spell = |Battle.(battle), spell| {
		spell_damage = spell.damage() ?? 0
		spell_heal = spell.heal() ?? 0

		buff_clocks = match spell.effect() {
			Ok(Shield) => { ..battle.player.buff_clocks, shield: 6 }
			Ok(Poison) => { ..battle.player.buff_clocks, poison: 6 }
			Ok(Recharge) => { ..battle.player.buff_clocks, recharge: 5 }
			Err(None) => battle.player.buff_clocks
		}

		player = Player.(
			{
				armor: battle.player.armor,
				hp: battle.player.hp + spell_heal.to_i64_wrap(),
				mana: battle.player.mana - spell.cost(),
				spent_mana: battle.player.spent_mana + spell.cost(),
				buff_clocks,
				spells_cast: battle.player.spells_cast.append(spell),
			},
		)

		boss = {
			..battle.boss,
			hp: battle.boss.hp - spell_damage.to_i64_wrap(),
		}

		Battle.({ boss, player, hard_mode: battle.hard_mode })
	}

	tick_buffs : Battle -> Battle
	tick_buffs = |Battle.(battle)| {
		old_buffs = battle.player.buff_clocks

		(armor, shield) =
			if old_buffs.shield > 0
				(7, old_buffs.shield - 1)
			else
				(0, 0)

		(damage, poison) =
			if old_buffs.poison > 0
				(3, old_buffs.poison - 1)
			else
				(0, 0)

		(bonus_mana, recharge) =
			if old_buffs.recharge > 0
				(101, old_buffs.recharge - 1)
			else
				(0, 0)

		buff_clocks = { shield, poison, recharge }
		boss = { ..battle.boss, hp: battle.boss.hp - damage }

		Player.(old_player) = battle.player
		player = Player.(
			{
				..old_player,
				armor,
				buff_clocks,
				mana: battle.player.mana + bonus_mana,
			},
		)

		Battle.({ boss, player, hard_mode: battle.hard_mode })
	}

	boss_attack : Battle -> Battle
	boss_attack = |Battle.(battle)| {
		effective_damage = (battle.boss.damage.to_i64_wrap() - battle.player.armor.to_i64_wrap()).max(1)

		player = { ..battle.player, hp: battle.player.hp - effective_damage }

		Battle.({ ..battle, player })
	}

	hard_mode_bleed : Battle -> Battle
	hard_mode_bleed = |Battle.(battle)| {
		if !battle.hard_mode return Battle.(battle)

		player = { ..battle.player, hp: battle.player.hp - 1 }
		Battle.({ ..battle, player })
	}

	execute_round : Battle, Spell -> Outcome
	execute_round = |battle, spell|
		battle
			.hard_mode_bleed()
			.check_victory()?
			.tick_buffs()
			.check_victory()?
			.cast_spell(spell)
			.check_victory()?
			.tick_buffs()
			.check_victory()?
			.boss_attack()
			.check_victory()

	check_victory : Battle -> Outcome
	check_victory = |Battle.(battle)| {
		if battle.boss.hp <= 0 {
			victory = {
				mana_spent: battle.player.spent_mana,
				spells_cast: battle.player.spells_cast,
			}
			return Err(Win(victory))
		}

		if battle.player.hp <= 0
			return Err(Loss)

		Ok(Battle.(battle))
	}

	next_rounds : Battle -> List(Outcome)
	next_rounds = |battle|
		battle.player.castable_spells()
			.map(|s| battle.execute_round(s))
}

# the outcome of one round of battle
# a win includes the player's final spent mana
Outcome : Try(Battle, [Win(Victory), Loss])

Victory : { mana_spent : U64, spells_cast : List(Spell) }

Boss := { hp : I64, damage : U64 }

Player := {
	hp : I64,
	mana : U64,
	spent_mana : U64,
	armor : U64,

	# turns remaining; 0 = none
	buff_clocks : {
		shield : U64,
		poison : U64,
		recharge : U64,
	},

	spells_cast : List(Spell),
}.{
	new = Player.(
		{
			hp: player_hp,
			mana: player_mana,
			spent_mana: 0,
			armor: 0,
			buff_clocks: { shield: 0, poison: 0, recharge: 0 },
			spells_cast: [],
		},
	)

	castable_spells : Player -> List(Spell)
	castable_spells = |player|
		Spell.all
			.drop_if(|s| s.cost() > player.mana)
			.drop_if(|s| s == Shield and player.buff_clocks.shield > 1)
			.drop_if(|s| s == Poison and player.buff_clocks.poison > 1)
			.drop_if(|s| s == Recharge and player.buff_clocks.recharge > 1)
}

solve : { hard_mode : Bool } -> U64
solve = |{ hard_mode }| {
	var $lowest_mana_spend = U64.highest

	var $battles = {
		Ok(boss) = parse_boss_stats(puzzle_input)
		player = Player.new

		[Battle.({ player, boss, hard_mode })]
	}

	while True {
		battle = match $battles.last() {
			Ok(b) => b
			Err(_) => break
		}

		var $next_rounds = []
		for outcome in battle.next_rounds() match outcome {
			Ok(next) if next.player.spent_mana < $lowest_mana_spend => {
				$next_rounds = $next_rounds.append(next)
			}

			Err(Win(victory)) if victory.mana_spent < $lowest_mana_spend => {
				expect victory.mana_spent == victory.spells_cast.iter().map(Spell.cost).sum()
				$lowest_mana_spend = victory.mana_spent
				# dbg { victory: victory }
			}

			_loss_or_worse_win => {}
		}

		existing_rounds =
			if $battles.is_empty()
				[]
			else
				$battles.take_first($battles.len() - 1)

		$battles = existing_rounds.concat($next_rounds)
	}

	$lowest_mana_spend
}
