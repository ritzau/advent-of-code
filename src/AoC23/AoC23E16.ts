import { readFileSync } from "fs"
import assert from "node:assert"
import { logResult } from "./aoclib"

export async function main() {
    logResult("Part 1 - sample", part1('AoC23E16-sample.txt'), 46)
    logResult("Part 1 - input", part1('AoC23E16-input.txt'), 7632)
    logResult("Part 2 - sample", part2('AoC23E16-sample.txt'), 51)
    logResult("Part 2 - input", part2('AoC23E16-input.txt'), 8023)
}

function part1(path: string) {
    const map = parseMap(path)
    return stepBeam(map, { r: 0, c: 0, d: Direction.RIGHT })
}

function part2(path: string) {
    const map = parseMap(path)
    let max = 0

    for (let r = 0; r < map.length; ++r) {
        const energizedLeft = stepBeam(map, { r: r, c: 0, d: Direction.RIGHT })
        const energizedRight = stepBeam(map, { r: r, c: map[r].length - 1, d: Direction.LEFT })
        max = Math.max(max, energizedLeft, energizedRight)
    }

    for (let c = 0; c < map[0].length; ++c) {
        const energizedTop = stepBeam(map, { r: 0, c: c, d: Direction.DOWN })
        const energizedBottom = stepBeam(map, { r: map.length - 1, c: c, d: Direction.UP })
        max = Math.max(max, energizedTop, energizedBottom)
    }

    return max
}


function parseMap(path: string) {
    return readFileSync(path).toString()
        .split('\n')
        .filter(l => l.length > 0)
        .map(l => Array.from(l))
}

enum Direction { UP, DOWN, LEFT, RIGHT }
type Step = { r: number, c: number, d: Direction }

function stepBeam(map: string[][], startStep: Step) {
    const visited = new Set<number>()
    const queue = [startStep]

    while (true) {
        const step = queue.shift()
        if (step === undefined) break

        const { r, c, d } = step
        if (r < 0 || map.length <= r) continue
        if (c < 0 || map[r].length <= c) continue

        const stepId = r * 1000000 + c * 1000 + d
        if (visited.has(stepId)) continue
        visited.add(stepId)

        const tile = map[r][c]
        switch (tile) {
            case '^':
            case 'v':
            case '<':
            case '>':
            case '.':
                map[r][c] = directionSymbol(d)
                queue.push(next(step, d))
                break
            case '/':
                queue.push(next(step, slashReflection(d)))
                break
            case '\\':
                queue.push(next(step, backslashReflection(d)))
                break
            case '|':
                if (d == Direction.LEFT || d == Direction.RIGHT) {
                    queue.push(next(step, Direction.UP))
                    queue.push(next(step, Direction.DOWN))
                } else {
                    queue.push(next(step, d))
                }
                break
            case '-':
                if (d == Direction.UP || d == Direction.DOWN) {
                    queue.push(next(step, Direction.LEFT))
                    queue.push(next(step, Direction.RIGHT))
                } else {
                    queue.push(next(step, d))
                }
                break
            default: assert.fail()
        }
    }

    return new Set(Array.from(visited).map((id => Math.trunc(id / 1000)))).size

    function slashReflection(d: Direction) {
        switch (d) {
            case Direction.UP: return Direction.RIGHT
            case Direction.DOWN: return Direction.LEFT
            case Direction.LEFT: return Direction.DOWN
            case Direction.RIGHT: return Direction.UP
        }
    }

    function backslashReflection(d: Direction) {
        switch (d) {
            case Direction.UP: return Direction.LEFT
            case Direction.DOWN: return Direction.RIGHT
            case Direction.LEFT: return Direction.UP
            case Direction.RIGHT: return Direction.DOWN
        }
    }

    function next(step: Step, direction: Direction) {
        const { dr, dc } = deltas(direction)
        return { r: step.r + dr, c: step.c + dc, d: direction }
    }

    function deltas(d: Direction) {
        switch (d) {
            case Direction.UP: return { dr: -1, dc: 0 }
            case Direction.DOWN: return { dr: 1, dc: 0 }
            case Direction.LEFT: return { dr: 0, dc: -1 }
            case Direction.RIGHT: return { dr: 0, dc: 1 }
        }
    }

    function directionSymbol(d: Direction) {
        switch (d) {
            case Direction.UP: return '^'
            case Direction.DOWN: return 'v'
            case Direction.LEFT: return '<'
            case Direction.RIGHT: return '>'
        }
    }
}
