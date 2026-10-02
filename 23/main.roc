import "./input.txt" as puzzle_input : Str

main! : List(Str) => Try({}, _)
main! = |_args| {
	program = Program.parse(puzzle_input)?

	dbg program

	Ok({})
}

Program := List(Instruction).{
	parse : Str -> Try(Program, _)
	parse = |input| {
		instructions = input
			.trim()
			.split_on("\n")
			.map_try(Instruction.parse)?

		dbg instructions

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
		["tpl", r] => {
			register = Register.parse(r)?
			Ok(Instruction.(Triple(register)))
		}

		["inc", r] => {
			register = Register.parse(r)?
			Ok(Instruction.(Increment(register)))
		}

		["hlf", r] => {
			register = Register.parse(r)?
			Ok(Instruction.(Half(register)))
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
}

expect Instruction.parse("tpl a") == Ok(Instruction.(Triple(A)))
expect Instruction.parse("tpl b") == Ok(Instruction.(Triple(B)))
expect Instruction.parse("inc a") == Ok(Instruction.(Increment(A)))
expect Instruction.parse("hlf a") == Ok(Instruction.(Half(A)))
expect Instruction.parse("jmp +2") == Ok(Instruction.(Jump(2)))
expect Instruction.parse("jmp -7") == Ok(Instruction.(Jump(-7)))
expect Instruction.parse("jie a, +4") == Ok(Instruction.(JumpIfEven(A, 4)))
expect Instruction.parse("jio a, +8") == Ok(Instruction.(JumpIfOne(A, 8)))

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

Machine := { pc : U64, a : I32, b : I32 }.{
	is_eq : _

	default : () -> Machine
	default = || Machine.({ pc: 0, a: 0, b: 0 })

	run : Machine, Program -> Machine
	run = |_machine, _program| {
		crash "todo"
	}
}
