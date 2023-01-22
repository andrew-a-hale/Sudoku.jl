# decompose modules into many smaller modules?
# Sudoku
#   Solvers
#   Helpers
#   Validators
#   Precompute
module SudokuSolver

# Data Model
struct Config{T<:UInt8,S<:Array{UInt8}}
	zero::T
	one::T
	size::T
	rows::T
	value_set::S
	subgridsize::T
end

# bad data types?
mutable struct Sudoku{T<:Array{UInt8},S<:Array{String,1},R<:Array{Tuple{UInt8,UInt8},1}}
	current_grid::T
	fail_states::S
	changes::R
end

# Processing
function read_sudoku(input::String)
	input_subgridsize = sqrt(sqrt(length(input)))
	if (!isinteger(input_subgridsize))
		error("give sudoku has unsupported dimension")
	end
	parsed_grid = map(x -> parse(UInt8, x), split(replace(input, '.' => '0'), ""))
	Sudoku(parsed_grid, String[], Tuple{UInt8,UInt8}[])
end

# Precomputation
function precomputation()
	context_indexes()
end

# Precomputation Helpers
function row_indexes()
	range_offset = config.rows - config.one
	map(
		x -> range(x * config.rows - range_offset, length = config.rows),
		range(config.one, length = config.rows),
	)
end

function col_indexes()
	map(
		x -> range(x, step = config.rows, length = config.rows),
		range(config.one, length = config.rows),
	)
end

## looks inefficient
function subgrid_indexes()
	function mapper(x)
		row_start = floor_nearest(row_from_cell(x), config.subgridsize)
		col_start = floor_nearest(col_from_cell(x), config.subgridsize)
		subgrid_rows = range(row_start, step = config.one, length = config.subgridsize)
		subgrid_cols = range(col_start, step = config.one, length = config.subgridsize)
		row_indx = row_indexes()
		reduce(vcat, map(x -> x[subgrid_cols], row_indx[subgrid_rows]))
	end
	map(x -> mapper(x), range(config.one, length = config.size))
end

## looks inefficient
function context_indexes()
	map(
		cell -> vcat(
			row_indexes()[row_from_cell(cell)],
			col_indexes()[col_from_cell(cell)],
			subgrid_indexes()[cell],
		),
		range(config.one, length = config.size),
	)
end

function setup_config(sudoku::Sudoku)
	gridsize::UInt8 = length(sudoku.current_grid)
	rows::UInt8 = sqrt(gridsize)
	value_set::Array{UInt8} = range(0x01, length = rows)
	subgridsize::UInt8 = sqrt(rows)
	Config(0x00, 0x01, gridsize, rows, value_set, subgridsize)
end

# Solver
function run_solver(sudoku::Sudoku)
	iter = 0
	while !is_solved(sudoku)
		iter += 1
		if iter % 10000 == 0
			@show iter
		end
		sudoku = solve(sudoku)
	end
	@show iter
	sudoku
end

## decompose into smaller functions
function solve(sudoku::Sudoku)
	if is_solved(sudoku)
		return sudoku
	end

	# need to filter candidates
	# if in a whole row, col, or grid a cell value is unique it is the only candidate for cell
	all_candidates = map(
		cell -> get_candidates(sudoku.current_grid, cell),
		range(config.one, length = config.size),
	)

	# rewrite as function
	cell_to_change::UInt8 = findfirst(!isnothing, all_candidates)
	for cell::UInt8 ∈ eachindex(all_candidates)
		if !isnothing(all_candidates[cell])
			if length(all_candidates[cell]) < length(all_candidates[cell_to_change])
				cell_to_change = cell
			end
		end
	end
	new_cell_candidates = all_candidates[cell_to_change]

	# rewrite as validator?
	error =
		!isnothing(findfirst(
			cell ->
				isnothing(all_candidates[cell]) && sudoku.current_grid[cell] == config.zero,
			range(config.one, length = config.size),
		))

	# rewrite as validator
	if length(new_cell_candidates) == 1
		proposed_change = deepcopy(sudoku.current_grid)
		proposed_change[cell_to_change] = new_cell_candidates[1]
		if failed_state(proposed_change)
			error = true
		end
	end

	if error
		return handle_error_state(sudoku)
	end

	if length(new_cell_candidates) == 1
		sudoku = make_move(sudoku, (cell_to_change, new_cell_candidates[1]))
	end

	if length(new_cell_candidates) > 1 && config.zero ∈ sudoku.current_grid
		sudoku = make_guess(sudoku, cell_to_change)
	end

	return sudoku
end

# Solve Subroutines
function failed_state(sudoku::Sudoku)
	join(sudoku.current_grid) ∈ sudoku.fail_states
end

function failed_state(grid::Array{UInt8})
	join(grid) ∈ sudoku.fail_states
end

function update_fail_states(sudoku)
	if !failed_state(sudoku)
		push!(sudoku.fail_states, join(sudoku.current_grid))
	end
	sudoku
end

function update_changes(sudoku, last_change)
	push!(sudoku.changes, last_change)
	sudoku
end

function revert_last_change(sudoku)
	cell, value = pop!(sudoku.changes)
	sudoku.current_grid[cell] = config.zero
	sudoku
end

function handle_error_state(sudoku)
	sudoku |> update_fail_states |> revert_last_change
end

function get_candidates(grid, cell)
	if grid[cell] > config.zero
		return nothing
	end
	context = get_context(grid, cell)
	candidates = setdiff(config.value_set, context)
	length(candidates) > 0 ? candidates : nothing
end

function make_guess(sudoku, cell)
	candidates = get_candidates(sudoku.current_grid, cell)
	grid = deepcopy(sudoku.current_grid)
	if !isnothing(candidates)
		for candidate ∈ candidates
			grid[cell] = candidate
			if !failed_state(grid)
				return make_move(sudoku, (cell, candidate))
			end
		end
	end
	# in fail state
	handle_error_state(sudoku)
end

function make_move(sudoku, move)
	cell, value = move
	sudoku.current_grid[cell] = value
	update_changes(sudoku, move)
end

# Validators
function is_solved(sudoku::Sudoku)
	validate_grid(sudoku.current_grid)
end

function validate_grid(grid)
	if config.zero ∈ grid
		return false
	end
	all(
		i -> validate_context(get_context(grid, i)),
		range(config.one, length = config.size),
	)
end

function validate_context(context)
	row, col, subgrid = collect(Iterators.partition(context, config.rows))
	all(i -> length(unique(context[i])) == config.rows, [row, col, subgrid])
end

# Helpers
function row_from_cell(cell)
	(cell - config.one) ÷ config.rows + config.one
end

function col_from_cell(cell)
	(cell - config.one) % config.rows + config.one
end

function floor_nearest(x, n)
	(((x - config.one) ÷ n) * n) + config.one
end

function get_context(grid, cell)
	grid[cell_contexts[cell]]
end

# Print
function print(sudoku::Sudoku)
	for i ∈ range(config.one, length = config.size)
		if i % config.rows === config.zero
			println(sudoku.current_grid[i])
		else
			Base.print(sudoku.current_grid[i], " ")
		end
	end
end

# Example
# const example_grid = ".....6....59.....82....8....45........3........6..3.54...325..6.................."
# const example_grid = "080070000000009300100000200002000400500080007003000900001000005009400000000010060"
# const example_grid = "100800040080010060300060100008003700000208000005400900009040000050030070030006004"
const example_grid = "900000100000003070605080003000408020800090001030107000500010908040500000002000006"
const sudoku = read_sudoku(example_grid)
const config = setup_config(sudoku)
const cell_contexts = precomputation()
@time print(run_solver(sudoku))

end
