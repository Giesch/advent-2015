import "./input.txt" as puzzle_input : Str

parsed_input = parse_input(puzzle_input)

parse_input : Str -> List(Str)
parse_input = |input|
	input.trim().split_on(",")

main! : List(Str) => Try({}, _)
main! = |args| {
	part_one = solve_part_one(args)?
	echo!("Part One: ${part_one.to_str()}\n")

	Ok({})
}

solve_part_one = |_| {
	machine = Machine.new(parsed_input, [1])
	output = machine.run()?

	error_codes = output.drop_last(1)
	expect error_codes.all(|e| e == 0)

	diagnostic = output.last() ? EmptyOutput

	Ok(diagnostic)
}

OpCode := [
	Add,
	Multiply,
	Input,
	Output,
	Halt,
].{
	parse : Str -> Try(OpCode, _)
	parse = |str| match U64.from_str(str)? {
		1 => Ok(Add)
		2 => Ok(Multiply)
		3 => Ok(Input)
		4 => Ok(Output)

		99 => Ok(Halt)

		other => Err(InvalidOpCode(other))
	}
}

Param := [Position(U64), Immediate(I64)].{
	parse = |str, mode| match mode {
		Position => U64.from_str(str).map_ok(|pos| Position(pos))
		Immediate => I64.from_str(str).map_ok(|val| Immediate(val))
	}

	resolve : Param, List(Str) -> Try(I64, _)
	resolve = |param, program| {
		match param {
			Immediate(val) => Ok(val)
			Position(pos) => {
				str = program.get(pos) ? InvalidAddr
				val = I64.from_str(str)?
				Ok(val)
			}
		}
	}
}

WriteParam := [Position(U64)].{
	parse = |str| U64.from_str(str).map_ok(|pos| Position(pos))
}

ascii_zero = 48.U8

ParamMode := [Position, Immediate].{
	is_eq : _
}

Instruction := [
	Add(Param, Param, WriteParam),
	Multiply(Param, Param, WriteParam),
	Input(WriteParam),
	Output(Param),
	Halt,
].{
	parse : List(Str), U64 -> Try(Instruction, _)
	parse = |program, pc| {
		bytes = {
			str = program.get(pc) ? InvalidPC
			raw_bytes = str.to_utf8()
			List.repeat(ascii_zero, 4).concat(raw_bytes)
		}

		leading_chars = bytes.drop_last(2)

		get_param_mode : U64 -> Try(ParamMode, _)
		get_param_mode = |param_pos| {
			l = leading_chars.len().to_u64_try()?
			i = l - (param_pos + 1)

			match leading_chars.get(i) ? InvalidParamAddr {
				byte if byte == ascii_zero => Ok(Position)
				byte if byte == ascii_zero + 1 => Ok(Immediate)
				other => Err(InvalidParamMode(other))
			}
		}

		opcode = {
			last_two_chars = Str.from_utf8(bytes.take_last(2))?
			OpCode.parse(last_two_chars)?
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

		match opcode {
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

			Halt => Ok(Halt)
		}
	}

	len : Instruction -> U64
	len = |instruction| match instruction {
		Add(_, _, _) => 4
		Multiply(_, _, _) => 4

		Input(_) => 2
		Output(_) => 2

		Halt => 1
	}
}

Machine := {
	pc : U64,
	program : List(Str),
	input : List(I64),
	output : List(I64),
}.{
	new : List(Str), List(I64) -> Machine
	new = |program, input|
		Machine.({ pc: 0, program, input, output: [] })

	run : Machine -> Try(List(I64), _)
	run = |machine|
		match machine.step() {
			Err(err) => Err(err)
			Ok(Done(output)) => Ok(output)
			Ok(Continue(next_machine)) => next_machine.run()
		}

	step : Machine -> Try([Done(List(I64)), Continue(Machine)], _)
	step = |Machine.({ pc, program, input, output } as m)| {
		instruction = Instruction.parse(program, pc)?

		continue = |properties| {
			new_pc = pc + instruction.len()
			Ok(Continue(Machine.({ ..properties, pc: new_pc })))
		}

		match instruction {
			Add(l, r, Position(w)) => {
				left = l.resolve(program)?
				right = r.resolve(program)?
				val = left + right

				new_program = program.set(w, val.to_str())?

				continue({ ..m, program: new_program })
			}

			Multiply(l, r, Position(w)) => {
				left = l.resolve(program)?
				right = r.resolve(program)?
				val = left * right

				new_program = program.set(w, val.to_str())?

				continue({ ..m, program: new_program })
			}

			Input(Position(w)) => {
				val = input.get(0) ? MissingInput
				new_input = input.drop_first(1)
				new_program = program.set(w, val.to_str())?

				continue({ ..m, program: new_program, input: new_input })
			}

			Output(p) => {
				param = p.resolve(program)?
				new_output = output.append(param)

				continue({ ..m, output: new_output })
			}

			Halt => Ok(Done(output))
		}
	}
}

expect solve_part_one([]) == Ok(13787043)
