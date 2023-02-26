module Sudoku

include("PeterNorvigSudokuSolver.jl")
using .PNSS

include("SudokuSolver.jl")
using .SudokuSolver

# Example
const example_grid = ".....6....59.....82....8....45........3........6..3.54...325..6.................."
# const example_grid = "080070000000009300100000200002000400500080007003000900001000005009400000000010060"
# const example_grid = "100800040080010060300060100008003700000208000005400900009040000050030070030006004"
# const example_grid = "900000100000003070605080003000408020800090001030107000500010908040500000002000006"
const sudoku = SudokuSolver.read_sudoku(example_grid)
@time SudokuSolver.print(SudokuSolver.run_solver(sudoku))


end