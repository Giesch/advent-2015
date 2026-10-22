# Input table
#    |    1         2         3         4         5         6
# ---+---------+---------+---------+---------+---------+---------+
#  1 | 20151125  18749137  17289845  30943339  10071777  33511524
#  2 | 31916031  21629792  16929656   7726640  15514188   4041754
#  3 | 16080970   8057251   1601130   7981243  11661866  16474243
#  4 | 24592653  32451966  21345942   9380097  10600672  31527494
#  5 |    77061  17552253  28094349   6899651   9250759  31663883
#  6 | 33071741   6796745  25397450  24659492   1534922  27995004

Cell : { row : U64, col : U64, entry : U64 }

cell6x6 : Cell
cell6x6 = { row: 6, col: 6, entry: 27995004 }

# (row, column) of our desired output cell
target : (U64, U64)
target = (2981, 3075)

main! : List(Str) => Try({}, _)
main! = |_args| {
	partOne = solvePartOne()
	echo!("Part One: ${partOne.to_str()}\n")

	Ok({})
}

nextCell : Cell -> Cell
nextCell = |prev| {
	entry = (prev.entry * 252533).rem_by(33554393)

	(row, col) =
		if prev.row == 1
			(prev.col + 1, 1)
		else
			(prev.row - 1, prev.col + 1)

	{ row, col, entry }
}

solvePartOne : () -> U64
solvePartOne = || {
	var $current = cell6x6

	while ($current.row, $current.col) != target {
		$current = nextCell($current)
	}

	$current.entry
}

expect solvePartOne() == 9132360
