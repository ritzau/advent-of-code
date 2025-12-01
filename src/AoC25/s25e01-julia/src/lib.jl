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
        direction = line[1]
        distance = parse(Int, line[2:end])
        sign = line[1] == 'L' ? -1 : 1
        pos = mod(pos + sign * distance, 100) 
        if pos == 0
            zero_count += 1
        end
        # println("dir=$(line[1]) dist=$distance pos=$pos passwd=$zero_count")
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
        direction = line[1]
        distance = parse(Int, line[2:end])
        sign = line[1] == 'L' ? -1 : 1
        vpos = pos + sign * distance
        turns = abs(vpos÷100)
        if pos > 0 && vpos <= 0
            turns += 1
        end
        zero_count += turns
        pos = mod(vpos, 100) 
        # println("dir=$(line[1]) dist=$distance vpos=$vpos pos=$pos turns=$turns passwd=$zero_count")
    end

    return zero_count
end

end # module
