"""
AoC 2016 Day 1: No Time for a Taxicab
"""

module S16E01

export parse_input, solve_part1, solve_part2

@enum Direction North=1 East=2 South=3 West=4

function turn_right(dir::Direction)
    if dir == North
        return East
    elseif dir == East
        return South
    elseif dir == South
        return West
    else  # West
        return North
    end
end

function turn_left(dir::Direction)
    if dir == North
        return West
    elseif dir == West
        return South
    elseif dir == South
        return East
    else  # East
        return North
    end
end

function delta(dir::Direction)
    if dir == North
        return (0, 1)
    elseif dir == East
        return (1, 0)
    elseif dir == South
        return (0, -1)
    else  # West
        return (-1, 0)
    end
end

struct Instruction
    turn::Char
    blocks::Int
end

"""
Parse the input into a list of instructions
"""
function parse_input(input::String)
    instructions = Instruction[]
    for part in split(strip(input), ", ")
        turn = part[1]
        blocks = parse(Int, part[2:end])
        push!(instructions, Instruction(turn, blocks))
    end
    return instructions
end

"""
Solve part 1: Find Manhattan distance after following all instructions
"""
function solve_part1(input::String)
    instructions = parse_input(input)

    x, y = 0, 0
    direction = North

    for instruction in instructions
        direction = if instruction.turn == 'R'
            turn_right(direction)
        elseif instruction.turn == 'L'
            turn_left(direction)
        else
            error("Invalid turn: $(instruction.turn)")
        end

        dx, dy = delta(direction)
        x += dx * instruction.blocks
        y += dy * instruction.blocks
    end

    return abs(x) + abs(y)
end

"""
Solve part 2: Find Manhattan distance to first location visited twice
"""
function solve_part2(input::String)
    instructions = parse_input(input)

    x, y = 0, 0
    direction = North
    visited = Set([(0, 0)])

    for instruction in instructions
        direction = if instruction.turn == 'R'
            turn_right(direction)
        elseif instruction.turn == 'L'
            turn_left(direction)
        else
            error("Invalid turn: $(instruction.turn)")
        end

        dx, dy = delta(direction)

        for _ in 1:instruction.blocks
            x += dx
            y += dy

            if (x, y) in visited
                return abs(x) + abs(y)
            end
            push!(visited, (x, y))
        end
    end

    return 0
end

end # module
