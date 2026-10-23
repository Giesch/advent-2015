# (row, column) of our desired output cell
target = (2981.U64, 3075.U64)

main! : List(Str) => Try({}, _)
main! = |_args| {
	solution = solve()

	echo!("Part One: ${solution.to_str()}\n")

	Ok({})
}

solve : () -> U64
solve = || {
	# from the bottom right of the puzzle's starting table
	var $cell = { row: 6.U64, col: 6.U64, entry: 27995004.U64 }

	while ($cell.row, $cell.col) != target {
		entry = ($cell.entry * 252533).rem_by(33554393)
		(row, col) =
			if $cell.row == 1 ($cell.col + 1, 1)
			else ($cell.row - 1, $cell.col + 1)
		$cell = { row, col, entry }
	}

	$cell.entry
}

expect solve() == 9132360
