import "./input.txt" as puzzle_input : Str

Ok(parsed_input) = parse_input(puzzle_input)

parse_input : Str -> Try(List(I64), _)
parse_input = |input|
	input.trim().split_on(",").map_try(I64.from_str)

main! : List(Str) => Try({}, _)
main! = |args| {
	part_one = solve_part_one(args)?
	echo!("Part One: ${part_one.to_str()}\n")

	Ok({})
}

solve_part_one : List(Str) -> Try(I64, _)
solve_part_one = |_| {
	var $max = I64.lowest
	for settings in permutations([0.I64, 1, 2, 3, 4]) {
		output = check_settings({ program: parsed_input, settings })?
		$max = output.max($max)
	}

	Ok($max)
}

expect solve_part_one([]) == Ok(24405)

amp_count = 5.U64

check_settings : { program : List(I64), settings : List(I64) } -> Try(I64, _)
check_settings = |{ program, settings }| {
	expect settings.len() == amp_count

	empty_machine = Machine.new(program, [])
	amplifiers = List.repeat(empty_machine, amp_count)
		.map2(settings, |machine, setting| { machine, setting })

	var $input_signal = 0.I64
	var $current_machine = 0.U64
	while $current_machine < amplifiers.len() {
		{ machine, setting } = amplifiers.get($current_machine) ?? crash "unreachable"
		input = [setting, $input_signal]

		output = machine.send(input).run()?

		$input_signal = output.get(0) ? EmptyOutput
		$current_machine = $current_machine + 1
	}

	Ok($input_signal)
}

expect {
	program = parse_input("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")?
	settings = [4, 3, 2, 1, 0]
	expected = 43210
	check_settings({ program, settings }) == Ok(expected)
}

expect {
	program = parse_input("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0")?
	settings = [0, 1, 2, 3, 4]
	expected = 54321
	check_settings({ program, settings }) == Ok(expected)
}

expect {
	program = parse_input("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0")?
	settings = [1, 0, 4, 3, 2]
	expected = 65210
	check_settings({ program, settings }) == Ok(expected)
}

###############

OpCode := [
	Add,
	Multiply,
	Input,
	Output,
	JumpIfTrue,
	JumpIfFalse,
	LessThan,
	Equals,
	Halt,
].{
	is_eq : _

	parse : Str -> Try(OpCode, _)
	parse = |str| {
		int = U64.from_str(str) ? |_| BadOpCode(str)
		OpCode.from_int(int)
	}

	from_int : U64 -> Try(OpCode, _)
	from_int = |int| match int {
		1 => Ok(Add)
		2 => Ok(Multiply)
		3 => Ok(Input)
		4 => Ok(Output)
		5 => Ok(JumpIfTrue)
		6 => Ok(JumpIfFalse)
		7 => Ok(LessThan)
		8 => Ok(Equals)

		99 => Ok(Halt)

		other => Err(InvalidOpCode(other))
	}

}

InstructionHeader := {
	opcode : OpCode,
	modes : (ParamMode, ParamMode, ParamMode),
}.{
	is_eq : _

	parse : U64 -> Try(InstructionHeader, _)
	parse = |int| {
		opcode = int.mod_by(100) |> OpCode.from_int()?
		modes = (
			(int / 100).mod_by(10) |> ParamMode.parse()?,
			(int / 1000).mod_by(10) |> ParamMode.parse()?,
			(int / 10000).mod_by(10) |> ParamMode.parse()?,
		)

		Ok(InstructionHeader.({ opcode, modes }))
	}

	expect {
		opcode = Add # 01
		modes = (Position, Position, Position) # 000
		parse(1) == Ok(InstructionHeader.({ opcode, modes }))
	}

	expect {
		opcode = Add # 01
		modes = (Immediate, Immediate, Immediate) # 111
		parse(11101) == Ok(InstructionHeader.({ opcode, modes }))
	}

	expect {
		opcode = Add # 01
		modes = (Immediate, Position, Position) # 001 right-to-left
		parse(101) == Ok(InstructionHeader.({ opcode, modes }))
	}

	expect {
		opcode = Add # 01
		modes = (Position, Position, Immediate) # 100 right-to-left
		parse(10001) == Ok(InstructionHeader.({ opcode, modes }))
	}
}

Param := [Position(U64), Immediate(I64)].{
	parse : I64, ParamMode -> Try(Param, _)
	parse = |int, mode| match mode {
		Immediate => Ok(Immediate(int))
		Position => Ok(Position(int.to_u64_try() ? InvalidPosition))
	}

	resolve : Param, List(I64) -> Try(I64, _)
	resolve = |param, program| match param {
		Immediate(val) => Ok(val)
		Position(pos) => Ok(program.get(pos) ? InvalidAddr)
	}
}

WriteParam := [Position(U64)].{
	parse : I64 -> Try(WriteParam, _)
	parse = |int| Ok(Position(int.to_u64_try() ? InvalidPosition))
}

ParamMode := [Position, Immediate].{
	is_eq : _

	parse : U64 -> Try(ParamMode, _)
	parse = |int| match int {
		0 => Ok(Position)
		1 => Ok(Immediate)
		other => Err(InvalidParamMode(other))
	}
}

Instruction := [
	Add(Param, Param, WriteParam),
	Multiply(Param, Param, WriteParam),
	Input(WriteParam),
	Output(Param),
	JumpIfTrue(Param, Param),
	JumpIfFalse(Param, Param),
	LessThan(Param, Param, WriteParam),
	Equals(Param, Param, WriteParam),
	Halt,
].{
	parse : List(I64), U64 -> Try(Instruction, _)
	parse = |program, pc| {
		header : InstructionHeader
		header = {
			instr = program.get(pc) ? InvalidPC
			h = instr.to_u64_try() ? InvalidOpCode
			InstructionHeader.parse(h) ? InvalidHeader
		}

		get_param_mode : U64 -> Try(ParamMode, _)
		get_param_mode = |param_pos| match param_pos {
			0 => Ok(header.modes.0)
			1 => Ok(header.modes.1)
			2 => Ok(header.modes.2)

			other => Err(InvalidHeaderAccess(other))
		}

		parse_param : U64 -> Try(Param, _)
		parse_param = |i| {
			mode = get_param_mode(i)?
			str = program.get(pc + 1 + i)?
			Param.parse(str, mode)
		}

		parse_write_param : U64 -> Try(WriteParam, _)
		parse_write_param = |i| {
			mode = get_param_mode(i)?
			if (mode == Immediate)
				return Err(ImmediateModeWriteParam)

			str = program.get(pc + 1 + i)?
			WriteParam.parse(str)
		}

		match header.opcode {
			Add => {
				left_param = parse_param(0)?
				right_param = parse_param(1)?
				write_param = parse_write_param(2)?
				Ok(Add(left_param, right_param, write_param))
			}

			Multiply => {
				left_param = parse_param(0)?
				right_param = parse_param(1)?
				write_param = parse_write_param(2)?
				Ok(Multiply(left_param, right_param, write_param))
			}

			Input => {
				write_param = parse_write_param(0)?
				Ok(Input(write_param))
			}

			Output => {
				param = parse_param(0)?
				Ok(Output(param))
			}

			JumpIfTrue => {
				param = parse_param(0)?
				dest = parse_param(1)?
				Ok(JumpIfTrue(param, dest))
			}

			JumpIfFalse => {
				param = parse_param(0)?
				dest = parse_param(1)?
				Ok(JumpIfFalse(param, dest))
			}

			LessThan => {
				left_param = parse_param(0)?
				right_param = parse_param(1)?
				write_param = parse_write_param(2)?
				Ok(LessThan(left_param, right_param, write_param))
			}

			Equals => {
				left_param = parse_param(0)?
				right_param = parse_param(1)?
				write_param = parse_write_param(2)?
				Ok(Equals(left_param, right_param, write_param))
			}

			Halt => Ok(Halt)
		}
	}

	len : Instruction -> U64
	len = |instruction| match instruction {
		Add(_, _, _) => 4
		Multiply(_, _, _) => 4

		Input(_) => 2
		Output(_) => 2

		JumpIfTrue(_, _) => 3
		JumpIfFalse(_, _) => 3

		LessThan(_, _, _) => 4
		Equals(_, _, _) => 4

		Halt => 1
	}
}

Machine := {
	pc : U64,
	program : List(I64),
	input : List(I64),
	output : List(I64),
}.{
	new : List(I64), List(I64) -> Machine
	new = |program, input|
		Machine.({ pc: 0, program, input, output: [] })

	run : Machine -> Try(List(I64), _)
	run = |machine| match machine.step() {
		# TODO change this to make it easy to pause here
		# Err(MissingInput) => crash "TODO"
		Err(err) => Err(err)
		Ok(Done(output)) => Ok(output)
		Ok(Continue(next_machine)) => next_machine.run()
	}

	# TODO
	# we need to allow buffering multiple outputs,
	# and pausing to wait on missing inputs
	step : Machine -> Try([Done(List(I64)), Continue(Machine)], _)
	step = |Machine.({ pc, program, input, output } as machine)| {
		instruction = Instruction.parse(program, pc)?

		continue = |properties| {
			new_pc = pc + instruction.len()
			Ok(Continue(Machine.({ ..properties, pc: new_pc })))
		}

		jump_if = |condition, dest| {
			new_pc = if (condition) dest else pc + instruction.len()
			Ok(Continue(Machine.({ ..machine, pc: new_pc })))
		}

		match instruction {
			Add(l, r, Position(w)) => {
				left = l.resolve(program)?
				right = r.resolve(program)?
				val = left + right

				new_program = program.set(w, val)?

				continue({ ..machine, program: new_program })
			}

			Multiply(l, r, Position(w)) => {
				left = l.resolve(program)?
				right = r.resolve(program)?
				val = left * right

				new_program = program.set(w, val)?

				continue({ ..machine, program: new_program })
			}

			Input(Position(w)) => {
				val = input.get(0) ? MissingInput
				new_input = input.drop_first(1)
				new_program = program.set(w, val)?

				continue({ ..machine, program: new_program, input: new_input })
			}

			Output(p) => {
				param = p.resolve(program)?
				new_output = output.append(param)

				continue({ ..machine, output: new_output })
			}

			JumpIfTrue(p, d) => {
				param = p.resolve(program)?
				dest = d.resolve(program)?.to_u64_try()?

				jump_if(param != 0, dest)
			}

			JumpIfFalse(p, d) => {
				param = p.resolve(program)?
				dest = d.resolve(program)?.to_u64_try()?

				jump_if(param == 0, dest)
			}

			LessThan(l, r, Position(w)) => {
				left = l.resolve(program)?
				right = r.resolve(program)?
				val = if (left < right) 1 else 0

				new_program = program.set(w, val)?

				continue({ ..machine, program: new_program })
			}

			Equals(l, r, Position(w)) => {
				left = l.resolve(program)?
				right = r.resolve(program)?
				val = if (left == right) 1 else 0

				new_program = program.set(w, val)?

				continue({ ..machine, program: new_program })
			}

			Halt => Ok(Done(output))
		}
	}

	send : Machine, List(I64) -> Machine
	send = |Machine.(machine), new_inputs| {
		input = machine.input.concat(new_inputs)
		Machine.({ ..machine, input })
	}
}

permutations : List(item) -> Iter(List(item))
permutations = |list| {
	next_order : List(U64) -> Try(List(U64), [NoMore])
	next_order = |order| {
		len = order.len()
		if (len < 2)
			return Err(NoMore)

		at = |i| order.get(i) ?? crash "permutation index out of bounds"

		var $pivot_end = len - 1
		while $pivot_end > 0 and at($pivot_end - 1) >= at($pivot_end) {
			$pivot_end = $pivot_end - 1
		}
		if ($pivot_end == 0)
			return Err(NoMore)

		pivot = $pivot_end - 1
		pivot_value = at(pivot)

		var $successor = len - 1
		while at($successor) <= pivot_value {
			$successor = $successor - 1
		}

		swapped = order.swap(pivot, $successor)
			?? crash "permutation index out of bounds"
		{ before, others } = swapped.split_at(pivot + 1)

		Ok(before.concat(others.rev()))
	}

	first_order : List(U64)
	first_order = (0.U64).until(list.len()).collect()

	Iter.custom(
		Ok(first_order),
		Err(Unknown),
		|state| {
			order = state?
			item = order.map(|i| list.get(i) ?? crash "permutation index out of bounds")

			Ok((item, next_order(order)))
		},
	)
}

expect {
	items : List(I64)
	items = []

	permutations(items).collect() == Set.from_list([[]])
}

expect permutations([1]).collect() == Set.from_list([[1]])

expect permutations([1, 2]).collect() == Set.from_list([[1, 2], [2, 1]])

expect permutations([1, 2, 3]).collect() == Set.from_list([
	[1, 2, 3],
	[1, 3, 2],
	[2, 1, 3],
	[2, 3, 1],
	[3, 1, 2],
	[3, 2, 1],
])
