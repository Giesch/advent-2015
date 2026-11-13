import "./input.txt" as puzzle_input : Str

Ok(parsed_input) =
	parse_input(puzzle_input)

parse_input : Str -> Try(List(U64), _)
parse_input = |input|
	input.trim().split_on(",").map_try(U64.from_str)

main! : List(Str) => Try({}, _)
main! = |_args| {
	part_one = solve_part_one()?
	echo!("Part One: ${part_one.to_str()}\n")

	Ok({})
}

solve_part_one : () -> Try(U64, _)
solve_part_one = || {
	fixed_input = parsed_input.set(1, 12)?.set(2, 2)?

	Machine.new(fixed_input).run()
}

Machine := {
	pc : U64,
	program : List(U64),
}.{
	new : List(U64) -> Machine
	new = |program|
		Machine.({ pc: 0, program })

	run : Machine -> Try(U64, _)
	run = |machine|
		match machine.step() {
			Err(err) => Err(err)
			Ok(Done(result)) => Ok(result)
			Ok(Continue(next_machine)) => next_machine.run()
		}

	step : Machine -> Try([Done(U64), Continue(Machine)], _)
	step = |Machine.({ pc, program })| {
		opcode = program.get(pc)?
		instruction = Instruction.parse(opcode)?

		read_instruction_line = || {
			left_addr = program.get(pc + 1)?
			left_val = program.get(left_addr)?

			right_addr = program.get(pc + 2)?
			right_val = program.get(right_addr)?

			dest = program.get(pc + 3)?

			Ok({ left: left_val, right: right_val, dest })
		}

		updated_program : List(U64)
		updated_program =
			match instruction {
				Halt => {
					result = program.get(0) ?? crash "empty program"
					return Ok(Done(result))
				}

				Add => {
					{ left, right, dest } = read_instruction_line()
						? InvalidInstructionBody
					program.set(dest, left + right)
						? InvalidDestAddr
				}

				Multiply => {
					{ left, right, dest } = read_instruction_line()
						? InvalidInstructionBody
					program.set(dest, left * right)
						? InvalidDestAddr
				}
			}

		Ok(Continue(Machine.({ pc: pc + 4, program: updated_program })))
	}
}

Instruction := [Add, Multiply, Halt].{
	parse : U64 -> Try(Instruction, [InvalidOpCode(U64)])
	parse = |int| match int {
		1 => Ok(Add)
		2 => Ok(Multiply)
		99 => Ok(Halt)
		other => Err(InvalidOpCode(other))
	}
}

run_example = |example| {
	program = parse_input(example)?
	Machine.new(program).run()
}

expect {
	example = "1,9,10,3,2,3,11,0,99,30,40,50"
	result = run_example(example)?
	result == 3500
}

expect {
	example = "1,0,0,0,99"
	result = run_example(example)?
	result == 2
}

expect {
	example = "1,1,1,4,99,5,6,0,99"
	result = run_example(example)?
	result == 30
}

expect {
	result = solve_part_one()?
	result == 3760627
}
