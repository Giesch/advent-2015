main! : List(Str) => Try({}, _)
main! = |args| {
	solution = solve(args)

	echo!("Part One: ${solution.to_str()}\n")

	Ok({})
}

# (row, column) of our desired output cell
target = (2981.U64, 3075.U64)

Cell : { row : U64, col : U64, entry : U64 }

solve : List(Str) -> U64
solve = |_args| { # taking args to prevent compiler optimizing this to run at compile time
	var $cell = { row: 1.U64, col: 1.U64, entry: 20151125.U64 }

	while ($cell.row, $cell.col) != target {
		entry = ($cell.entry * 252533).rem_by(33554393)

		(row, col) =
			if ($cell.row == 1)
				($cell.col + 1, 1)
			else
				($cell.row - 1, $cell.col + 1)

		$cell = { row, col, entry }
	}

	$cell.entry
}

expect solve([]) == 9132360
