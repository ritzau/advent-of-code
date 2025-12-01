"""
Advent of Code solution
"""

module AoCSolution

export parse_input, solve_part1, solve_part2

"""
Parse the input into lines
"""
function parse_input(input::String)
    return split(strip(input), '\n')
end

"""
Solve part 1 of the puzzle
"""
function solve_part1(input::String)
    lines = parse_input(input)
    pos = 50
    zero_count = 0
    for line in lines
        sign = first(line) == 'L' ? -1 : 1
        distance = parse(Int, @view line[2:end])
        pos = mod(pos + sign * distance, 100)
        zero_count += (pos == 0)
    end

    return zero_count
end

"""
Solve part 2 of the puzzle
"""
function solve_part2(input::String)
    lines = parse_input(input)
    pos = 50
    zero_count = 0
    for line in lines
        sign = first(line) == 'L' ? -1 : 1
        distance = parse(Int, @view line[2:end])
        vpos = pos + sign * distance
        turns = abs(vpos ÷ 100)
        if pos > 0 && vpos <= 0
            turns += 1
        end
        zero_count += turns
        pos = mod(vpos, 100)
    end

    return zero_count
end

end # module
