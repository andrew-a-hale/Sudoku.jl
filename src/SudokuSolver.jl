module SudokuSolver

# Configuration
mutable struct Config{T<:UInt8}
    zero::T
    one::T
    size::T
    rows::T
    value_set::Array{UInt8}
    subgridsize::T
    context_indexes::Union{Missing, Any}
end

# Precomputation
function row_indexes(config::Config)
	range_offset = config.rows - config.one
	map(
		x -> range(x * config.rows - range_offset, length = config.rows),
		range(config.one, length = config.rows),
	)
end

function col_indexes(config::Config)
	map(
		x -> range(x, step = config.rows, length = config.rows),
		range(config.one, length = config.rows),
	)
end

function subgrid_indexes(config::Config)
	function mapper(x)
		row_start = floor_nearest(config, row_from_cell(config, x), config.subgridsize)
		col_start = floor_nearest(config, col_from_cell(config, x), config.subgridsize)
		subgrid_rows = range(row_start, step = config.one, length = config.subgridsize)
		subgrid_cols = range(col_start, step = config.one, length = config.subgridsize)
		row_indx = row_indexes(config)
		reduce(vcat, map(x -> x[subgrid_cols], row_indx[subgrid_rows]))
	end
	map(x -> mapper(x), range(config.one, length = config.size))
end

function context_indexes(config::Config)
	map(
		cell -> vcat(
			row_indexes(config)[row_from_cell(config, cell)],
			col_indexes(config)[col_from_cell(config, cell)],
			subgrid_indexes(config)[cell],
		),
		range(config.one, length = config.size),
	)
end

# Helpers
function row_from_cell(config, cell)
	(cell - config.one) ÷ config.rows + config.one
end

function col_from_cell(config, cell)
	(cell - config.one) % config.rows + config.one
end

function floor_nearest(config, x, n)
	(((x - config.one) ÷ n) * n) + config.one
end

function get_context(config, grid, cell)
	grid[config.context_indexes[cell]]
end

# Sudoku
mutable struct Sudoku{T<:Array{UInt8},S<:Array{String,1},R<:Array{Tuple{UInt8,UInt8},1}}
	config::Config
	current_grid::T
	fail_states::S
	changes::R
end

function print(sudoku::Sudoku)
	for i ∈ range(sudoku.config.one, length = sudoku.config.size)
		if i % sudoku.config.rows === sudoku.config.zero
			println(sudoku.current_grid[i])
		else
			Base.print(sudoku.current_grid[i], " ")
		end
	end
end

# Validators
function is_solved(sudoku::Sudoku)
	validate_grid(sudoku.config, sudoku.current_grid)
end

function validate_grid(config, grid)
	if config.zero ∈ grid
		return false
	end
	all(
		i -> validate_context(config, get_context(config, grid, i)),
		range(config.one, length = config.size),
	)
end

function validate_context(config, context)
	row, col, subgrid = collect(Iterators.partition(context, config.rows))
	all(i -> length(unique(context[i])) == config.rows, [row, col, subgrid])
end

# Processing
function read_sudoku(input::String)
    size::UInt8 = length(input)
    rows::UInt8 = sqrt(size)
	input_subgridsize::UInt8 = sqrt(rows)

	parsed_grid = map(x -> parse(UInt8, x), split(replace(input, '.' => '0'), ""))
    config = Config(0x00, 0x01, size, rows, Array{UInt8}(1:rows), input_subgridsize, missing)
    config.context_indexes = context_indexes(config)
	Sudoku(config, parsed_grid, String[], Tuple{UInt8,UInt8}[])
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

function solve(sudoku::Sudoku)
	if is_solved(sudoku)
		return sudoku
	end

	# need to filter candidates
	# if in a whole row, col, or grid a cell value is unique it is the only candidate for cell
	all_candidates = map(
		cell -> get_candidates(sudoku.config, sudoku.current_grid, cell),
		range(sudoku.config.one, length = sudoku.config.size),
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
				isnothing(all_candidates[cell]) && sudoku.current_grid[cell] == sudoku.config.zero,
			range(sudoku.config.one, length = sudoku.config.size),
		))

	# rewrite as validator
	if length(new_cell_candidates) == 1
		proposed_change = deepcopy(sudoku.current_grid)
		proposed_change[cell_to_change] = new_cell_candidates[1]
		if failed_state(proposed_change, sudoku)
			error = true
		end
	end

	if error
		return handle_error_state(sudoku)
	end

	if length(new_cell_candidates) == 1
		sudoku = make_move(sudoku, (cell_to_change, new_cell_candidates[1]))
	end

	if length(new_cell_candidates) > 1 && sudoku.config.zero ∈ sudoku.current_grid
		sudoku = make_guess(sudoku, cell_to_change)
	end

	return sudoku
end

# Solve Subroutines
function failed_state(sudoku::Sudoku)
	join(sudoku.current_grid) ∈ sudoku.fail_states
end

function failed_state(grid, sudoku)
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
	cell, _ = pop!(sudoku.changes)
	sudoku.current_grid[cell] = sudoku.config.zero
	sudoku
end

function handle_error_state(sudoku)
	sudoku |> update_fail_states |> revert_last_change
end

function get_candidates(config, grid, cell)
	if grid[cell] > config.zero
		return nothing
	end
	context = get_context(config, grid, cell)
	candidates = setdiff(config.value_set, context)
	length(candidates) > 0 ? candidates : nothing
end

function make_guess(sudoku, cell)
	candidates = get_candidates(sudoku.config, sudoku.current_grid, cell)
	grid = deepcopy(sudoku.current_grid)
	if !isnothing(candidates)
		for candidate ∈ candidates
			grid[cell] = candidate
			if !failed_state(grid, sudoku)
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

end