import assert from "node:assert"
import { readFileSync } from "node:fs"

import { logResult } from "./aoclib"

export async function main() {
    logResult("Part 1 - sample", part1("AoC23E18-sample.txt"), 62)
    logResult("Part 1 - input", part1("AoC23E18-input.txt"), 40761)
    logResult("Part 2 - sample", part2("AoC23E18-sample.txt"), 952408144115)
    logResult("Part 2 - input", part2("AoC23E18-input.txt"), 106920098354636)
}

interface StartVerticalLine {
    kind: "start"
    column: number
}

interface EndVerticalLine {
    kind: "end"
    column: number
}

interface HorizontalLine {
    kind: "horizontal"
    startColumn: number
    endColumn: number
}

type PointOfInterest = StartVerticalLine | EndVerticalLine | HorizontalLine

function part1(path: string) {
    const ops = parseOps(path)
    const bb = boundingBox(ops)
    const map = Array.from(
        { length: bb.maxY - bb.minY + 1 },
        (() => Array.from(
            { length: bb.maxX - bb.minX + 1 },
            () => '.'))
    )

    dig(map, ops, { x: -bb.minX, y: -bb.minY })

    const outer = ' '
    for (let y = 0; y < map.length; ++y) {
        floodFill(map, { x: 0, y }, '.', outer)
        floodFill(map, { x: map[y].length - 1, y }, '.', outer)
    }

    for (let x = 0; x < map[0].length; ++x) {
        floodFill(map, { x, y: 0 }, '.', outer)
        floodFill(map, { x, y: map.length - 1 }, '.', outer)
    }

    return map.reduce((a, line) =>
        a + line.reduce(
            (count, c) => c !== outer ? count + 1 : count,
            0)
        , 0)
}

function part2(path: string) {
    const ops = parseHexOps(path)
    const bb = boundingBox(ops)
    const pois = getPois(bb.minY, ops)
    return getArea(pois)
}

function getPois(minY: number, ops: Operation[]) {
    const pois: PointOfInterest[][] = []

    let pos = { x: 0, y: -minY }

    for (const op of ops) {
        const next = step(pos, op)
        switch (op.direction) {
            case Direction.LEFT:
            case Direction.RIGHT:
                getPoisForLine(pos.y).push({
                    kind: "horizontal",
                    startColumn: Math.min(pos.x, next.x),
                    endColumn: Math.max(pos.x, next.x),
                })
                break
            case Direction.UP:
            case Direction.DOWN:
                getPoisForLine(Math.min(pos.y, next.y) + 1).push({
                    kind: "start",
                    column: pos.x,
                })
                getPoisForLine(Math.max(pos.y, next.y)).push({
                    kind: "end",
                    column: pos.x,
                })
                break
            default:
                assert.fail()
        }
        pos = next
    }

    return pois

    function getPoisForLine(y: number) {
        if (pois[y] === undefined) {
            pois[y] = []
        }
        return pois[y]
    }
}

interface VerticalLineMark {
    kind: "vertical"
    column: number
}

interface HorizontalLineMark {
    kind: "horizontal"
    start: number
    end: number
}

type LineMark = VerticalLineMark | HorizontalLineMark

function getArea(pois: PointOfInterest[][]) {
    const marks: LineMark[] = []
    let previousLine: LineMark[] = []
    let previousY = -1
    let area = 0
    let repeatSum = 0

    pois.forEach((_, y) => {
        area += (y - previousY - 1) * repeatSum

        const repeatLine = renderRepeatLine(pois[y], Array.from(marks))!
        const currentLine = renderLine(pois[y], marks)!
        const nextLine = renderLine(pois[y + 1], Array.from(marks))

        area += countHoles(previousLine, currentLine, nextLine)
        repeatSum = countHoles(previousLine, repeatLine, nextLine)

        previousY = y
        previousLine = currentLine
    })

    return area
}

function renderLine(pois: PointOfInterest[], marks: LineMark[]) {
    if (pois === undefined) return undefined

    renderRepeatLine(pois, marks)

    let currentLine: LineMark[] = pois
        .filter(p => p.kind === "horizontal")
        .map(p => p as HorizontalLine)
        .map(poi => ({
            kind: "horizontal",
            start: poi.startColumn,
            end: poi.endColumn,
        }))

    currentLine.push(...marks)
    currentLine.sort(compareMarks)

    return currentLine
}

function renderRepeatLine(pois: PointOfInterest[], marks: LineMark[]) {
    if (pois === undefined) return undefined

    for (const poi of pois) {
        switch (poi.kind) {
            case "start":
                marks.push({ kind: "vertical", column: poi.column })
                break
            case "end":
                const index = marks.findIndex(elm => elm.kind === "vertical" && elm.column === poi.column)
                if (index !== -1) {
                    marks.splice(index, 1)
                } else {
                    assert.fail()
                }
                break
            case "horizontal":
                break
            default:
                assert.fail()
        }
    }

    marks.sort(compareMarks)

    return marks
}

function countHoles(previousLine: LineMark[], currentLine: LineMark[], nextLine: LineMark[] | undefined) {
    if (currentLine.length === 0) return 0

    let lineSum = 0
    let previousMark = currentLine[0]
    let inside = true

    if (previousMark.kind === "horizontal") {
        lineSum += previousMark.end - previousMark.start + 1
        if (!(hasOverlap(previousMark, previousLine) && hasOverlap(previousMark, nextLine))) {
            inside = !inside
        }
    }

    for (const mark of currentLine.slice(1)) {
        switch (mark.kind) {
            case "horizontal":
                if (inside) {
                    lineSum += mark.start - endOfMark(previousMark)
                }
                lineSum += mark.end - mark.start + 1
                if (hasOverlap(mark, previousLine) && hasOverlap(mark, nextLine)) {
                    inside = !inside
                }
                break
            case "vertical":
                if (inside) {
                    lineSum += mark.column - endOfMark(previousMark) + 1
                }
                inside = !inside
                break
            default: assert.fail()
        }
        previousMark = mark
    }

    return lineSum
}

function hasOverlap({ start, end }: HorizontalLineMark, pois: LineMark[] | undefined) {
    if (pois === undefined) {
        return false
    }

    const result = pois.some(poi => {
        switch (poi.kind) {
            case "vertical":
                return start <= poi.column && poi.column <= end
            case "horizontal":
                return start <= poi.start && end > poi.start || start <= poi.end && end > poi.end
            default:
                assert.fail()
        }
    })

    return result
}

function endOfMark(m: LineMark) {
    switch (m.kind) {
        case "horizontal": return m.end + 1
        case "vertical": return m.column
        default: assert.fail()
    }
}

function compareMarks(a: LineMark, b: LineMark) {
    return key(a) - key(b)

    function key(m: LineMark) {
        switch (m.kind) {
            case "vertical": return m.column
            case "horizontal": return m.start
            default: assert.fail()
        }
    }
}

function fmtMark(m: LineMark) {
    switch (m.kind) {
        case "vertical": return m.column.toString()
        case "horizontal": return `${m.start}-${m.end}`
        default: assert.fail()
    }
}

type Operation = {
    direction: Direction
    distance: number
}

enum Direction { RIGHT, LEFT, DOWN, UP }

type Position = {
    x: number
    y: number
}

function parseOps(path: string) {
    return readFileSync(path).toString()
        .split('\n')
        .filter(l => l.length > 0)
        .map(line => {
            const tokens = line.split(' ')
            assert.strictEqual(tokens.length, 3)
            const [direction, distance] = tokens
            return {
                direction: toDirection(direction),
                distance: parseInt(distance)
            }
        })
}

function parseHexOps(path: string) {
    return readFileSync(path).toString()
        .split('\n')
        .filter(l => l.length > 0)
        .map(line => {
            const match = line.match(/[RLDU] \d+ \(#([0-9a-f]{5})([0-3])\)/)
            if (match === null) assert.fail(line)
            const [, distance, direction] = match
            return {
                direction: toHexDirection(direction),
                distance: parseInt(distance, 16)
            }
        })
}

function toDirection(direction: string) {
    switch (direction) {
        case 'R': return Direction.RIGHT
        case 'L': return Direction.LEFT
        case 'D': return Direction.DOWN
        case 'U': return Direction.UP
        default: assert.fail()
    }
}

function toHexDirection(direction: string) {
    switch (direction) {
        case '0': return Direction.RIGHT
        case '1': return Direction.DOWN
        case '2': return Direction.LEFT
        case '3': return Direction.UP
        default: assert.fail()
    }
}

function boundingBox(ops: Operation[]) {
    let position = { x: 0, y: 0 }
    let minX = 0
    let maxX = 0
    let minY = 0
    let maxY = 0

    for (const op of ops) {
        position = step(position, op)

        minX = Math.min(minX, position.x)
        maxX = Math.max(maxX, position.x)
        minY = Math.min(minY, position.y)
        maxY = Math.max(maxY, position.y)
    }

    return { minX, maxX, minY, maxY }
}

function dig(map: string[][], ops: Operation[], start: Position) {
    let position = start
    for (const op of ops) {
        position = digTo(map, position, op)
    }
}

function digTo(map: string[][], position: Position, operation: Operation) {
    const destination = step(position, operation)
    assert(destination.x === position.x || destination.y === position.y)

    if (position.x === destination.x) {
        const from = Math.min(position.y, destination.y)
        const to = Math.max(position.y, destination.y)

        for (let i = from; i <= to; ++i) {
            map[i][position.x] = '#'
        }
    } else if (position.y === destination.y) {
        const from = Math.min(position.x, destination.x)
        const to = Math.max(position.x, destination.x)

        for (let i = from; i <= to; ++i) {
            map[position.y][i] = '#'
        }
    }

    return destination
}

function step({ x, y }: Position, { direction, distance }: Operation) {
    switch (direction) {
        case Direction.LEFT:
            return { x: x - distance, y }
        case Direction.RIGHT:
            return { x: x + distance, y }
        case Direction.UP:
            return { x, y: y - distance }
        case Direction.DOWN:
            return { x, y: y + distance }
        default:
            assert.fail()
    }
}

function floodFill(map: string[][], position: Position, background: string, color: string) {
    assert.strictEqual(color.length, 1)

    const queue = [position]
    while (true) {
        const p = queue.shift()
        if (p === undefined) break
        const { x, y } = p

        if (y < 0 || map.length <= y) continue
        if (x < 0 || map[y].length <= x) continue
        if (map[y][x] !== background) continue

        map[y][x] = color

        queue.push({ x: x + 1, y })
        queue.push({ x: x - 1, y })
        queue.push({ x, y: y - 1 })
        queue.push({ x, y: y + 1 })
    }
}
