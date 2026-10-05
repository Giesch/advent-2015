import "./input.txt" as puzzle_input : Str

Ok(input_program) = Program.parse(puzzle_input)

main! : List(Str) => Try({}, _)
main! = |_args| {
	part_one = Machine.default().run(input_program)
	echo!("Part One: ${part_one.b.to_str()}\n")

	part_two = Machine.({ pc: 0, a: 1, b: 0 }).run(input_program)
	echo!("Part Two: ${part_two.b.to_str()}\n")

	Ok({})
}

Program := List(Instruction).{
	parse : Str -> Try(Program, _)
	parse = |input| {
		instructions = input
			.trim()
			.split_on("\n")
			.map_try(Instruction.parse)?

		Ok(Program.(instructions))
	}
}

Instruction := [
	Half(Register),
	Triple(Register),
	Increment(Register),
	Jump(I32),
	JumpIfEven(Register, I32),
	JumpIfOne(Register, I32),
].{
	is_eq : _

	parse : Str -> Try(Instruction, _)
	parse = |line| match line.trim().split_on(" ") {
		["hlf", r] => {
			register = Register.parse(r)?
			Ok(Instruction.(Half(register)))
		}

		["tpl", r] => {
			register = Register.parse(r)?
			Ok(Instruction.(Triple(register)))
		}

		["inc", r] => {
			register = Register.parse(r)?
			Ok(Instruction.(Increment(register)))
		}

		["jmp", o] => {
			offset = I32.from_str(o) ? InvalidOffset
			Ok(Instruction.(Jump(offset)))
		}

		["jie", r, o] => {
			register = Register.parse(r)?
			offset = I32.from_str(o) ? InvalidOffset
			Ok(Instruction.(JumpIfEven(register, offset)))
		}

		["jio", r, o] => {
			register = Register.parse(r)?
			offset = I32.from_str(o) ? InvalidOffset
			Ok(Instruction.(JumpIfOne(register, offset)))
		}

		invalid => Err(InvalidInstruction(invalid))
	}

	expect parse("tpl a") == Ok(Instruction.(Triple(A)))
	expect parse("tpl b") == Ok(Instruction.(Triple(B)))
	expect parse("inc a") == Ok(Instruction.(Increment(A)))
	expect parse("hlf a") == Ok(Instruction.(Half(A)))
	expect parse("jmp +2") == Ok(Instruction.(Jump(2)))
	expect parse("jmp -7") == Ok(Instruction.(Jump(-7)))
	expect parse("jie a, +4") == Ok(Instruction.(JumpIfEven(A, 4)))
	expect parse("jio a, +8") == Ok(Instruction.(JumpIfOne(A, 8)))
}

Register := [A, B].{
	is_eq : _

	parse : Str -> Try(Register, _)
	parse = |token| {
		name = token.drop_suffix(",")

		match name {
			"a" => Ok(A)
			"b" => Ok(B)

			invalid => Err(InvalidRegister(invalid))
		}
	}
}

Machine := { pc : U64, a : I64, b : I64 }.{
	is_eq : _

	default : () -> Machine
	default = || Machine.({ pc: 0, a: 0, b: 0 })

	run : Machine, Program -> Machine
	run = |machine, program|
		match machine.step(program) {
			Err(Exit(_)) => machine
			Ok(new_machine) => new_machine.run(program)
		}

	step : Machine, Program -> Try(Machine, _)
	step = |Machine.({ pc, a, b }), Program.(instructions)| {
		instruction = instructions.get(pc) ? Exit

		next = { pc: pc + 1, a, b }

		ok = |registers| Ok(Machine.(registers))

		read : Register -> I64
		read = |register| match register {
			A => a
			B => b
		}

		write : Register, I64 -> [Ok(Machine)]
		write = |register, value| match register {
			A => ok({ ..next, a: value })
			B => ok({ ..next, b: value })
		}

		jump : I32 -> Try(Machine, [Exit([OutOfRange])])
		jump = |offset| {
			target = pc.to_i128() + offset.to_i128()
			new_pc = target.to_u64_try() ? Exit

			ok({ ..next, pc: new_pc })
		}

		jump_if : Bool, I32 -> Try(Machine, [Exit([OutOfRange])])
		jump_if = |condition, offset|
			if condition
				jump(offset)
			else
				Ok(Machine.(next))

		match instruction {
			Half(register) => write(register, read(register) / 2)
			Triple(register) => write(register, read(register) * 3)
			Increment(register) => write(register, read(register) + 1)

			Jump(offset) => jump(offset)

			JumpIfEven(register, offset) => jump_if(read(register).is_even(), offset)
			JumpIfOne(register, offset) => jump_if(read(register) == 1, offset)
		}
	}
}

expect {
	example =
		\\inc a
		\\jio a, +2
		\\tpl a
		\\inc a

	program = Program.parse(example)?

	result = Machine.default().run(program)

	result.a == 2
}
