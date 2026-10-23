main! : List(Str) => Try({}, _)
main! = |_args| {
	solution = solve()

	echo!("Part One: ${solution.to_str()}\n")

	Ok({})
}

# (row, column) of our desired output cell
target = (2981.U64, 3075.U64)

solve : () -> U64
solve = || {
	var $cell = { row: 1.U64, col: 1.U64, entry: 20151125.U64 }

	while ($cell.row, $cell.col) != target {
		entry = ($cell.entry * 252533).rem_by(33554393)

		(row, col) =
			if $cell.row == 1
				($cell.col + 1, 1)
			else
				($cell.row - 1, $cell.col + 1)

		$cell = { row, col, entry }
	}

	$cell.entry
}

expect solve() == 9132360
