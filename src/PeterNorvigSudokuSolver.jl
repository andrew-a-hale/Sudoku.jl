module PNSS

function cross(A, B)
    return [a * b for a ∈ A for b ∈ B]
end

const digits = "123456789"
const rows = "ABCDEFGHI"
const cols = digits
const squares = cross(rows, cols)
const unitlist = vcat(
    [cross(rows, c) for c ∈ cols],
    [cross(r, cols) for r ∈ rows],
    [cross(rs, cs) for rs ∈ ["ABC", "DEF", "GHI"] for cs ∈ ["123", "456", "789"]],
)
const units = Dict((s, [u for u ∈ unitlist if s ∈ u]) for s ∈ squares)
const peers = Dict((s, setdiff(Set(reduce(vcat, units[s])), Set([s]))) for s ∈ squares)

function grid_values(grid)
    values = Dict{String,String}()
    for d ∈ eachindex(squares)
        values[squares[d]] = string(grid[d])
    end
    values
end

function parse_grid(grid)
    vs = Dict((s, digits) for s ∈ squares)
    for (s, d) in grid_values(grid)
        if occursin(d, digits) && assign(vs, s, d) == false
            return false
        end
    end
    return vs
end

function assign(values, s, d)
    other_values = replace(values[s], d => "")
    if any([eliminate(values, s, d2) == false for d2 ∈ other_values])
        return false
    else
        return values
    end
end

function eliminate(values, s, d)
    if !occursin(d, values[s])
        return values
    end
    values[s] = replace(values[s], d => "")
    if length(values[s]) == 0
        return false
    elseif length(values[s]) == 1
        d2 = values[s]
        if any([eliminate(values, s2, d2) == false for s2 ∈ peers[s]])
            return false
        end
    end
    for u ∈ units[s]
        dplaces = [s for s ∈ u if occursin(d, values[s])]
        if length(dplaces) == 0
            return false
        elseif length(dplaces) == 1
            if assign(values, dplaces[1], d) == false
                return false
            end
        end
    end
    return values
end

function search(values)
    if values == false
        return false
    end

    if all(length(values[s]) == 1 for s ∈ squares)
        return values
    end

    s = squares[1]
    n = digits
    for i ∈ keys(values)
        if length(values[i]) < length(n)
            s = i
        end
    end
    return some(search(assign(deepcopy(values), s, d)) for d ∈ values[s])
end

function solve(grid)
    return search(parse_grid(grid))
end

function display(values)
    width = 1 + findmax([length(values[s]) for s ∈ squares])[1]
    for r ∈ rows
        for c ∈ cols
            if c == last(cols)
                println(center(values[r*c], width))
            else
                print(center(values[r*c], width))
            end
        end
    end
end

function some(req)
    for e ∈ req
        if e != false
            return e
        end
    end
    return false
end

function center(str, width)
    pad = width - length(str)
    lrpad = pad ÷ 2
    lpad(rpad(str, length(str) + lrpad), width)
end

@time display(solve("900000100000003070605080003000408020800090001030107000500010908040500000002000006"))

end
