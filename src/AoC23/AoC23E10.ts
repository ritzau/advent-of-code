import * as readline from "node:readline"

import { createReadStream } from "fs"
import { logResult, sum } from "./aoclib"
import assert from "node:assert"

export async function main() {
    logResult("Part 1 - sample 1", await part1('AoC23E10-sample-1.txt'), 8)
    logResult("Part 1 - input", await part1('AoC23E10-input.txt'), 6754)
    logResult("Part 2 - sample 2a", await part2('AoC23E10-sample-2a.txt'), 4)
    logResult("Part 2 - sample 2b", await part2('AoC23E10-sample-2b.txt'), 4)
    logResult("Part 2 - sample 3", await part2('AoC23E10-sample-3.txt'), 8)
    logResult("Part 2 - sample 4", await part2('AoC23E10-sample-4.txt'), 10)
    logResult("Part 2 - input", await part2('AoC23E10-input.txt'), 567)
}

enum TileType {
    VERTICAL,
    HORIZONTAL,
    NORTH_EAST,
    NORTH_WEST,
    SOUTH_WEST,
    SOUTH_EAST,
    GROUND,
    START,
}

enum TilePosition {
    INSIDE,
    PIPE,
    OUTSIDE,
}

type Tile = {
    type: TileType
    distance: number
    position: TilePosition | undefined
}

type TileMap = Tile[][]

async function part1(path: string) {
    const map = walkMap(await parseMapFromFile(path))
    return Math.max(...map.flatMap(r => r.map(t => t.distance).filter(x => !Number.isNaN(x))))
}

async function part2(path: string) {
    const map = walkMap(await parseMapFromFile(path))

    for (let row = 0; row < map.length; ++row) {
        for (let col = 0; col < map[row].length; ++col) {
            const t = map[row][col]
            if (!Number.isNaN(t.distance)) t.position = TilePosition.PIPE
        }
    }

    const expandedMap = expandMap(map)

    for (let row = 0; row < expandedMap.length; ++row) {
        floodFill(expandedMap, row, 0, TilePosition.OUTSIDE)
        floodFill(expandedMap, row, expandedMap[row].length - 1, TilePosition.OUTSIDE)
    }

    for (let i = 0; i < expandedMap[0].length; ++i) {
        floodFill(expandedMap, 0, i, TilePosition.OUTSIDE)
        floodFill(expandedMap, expandedMap.length - 1, i, TilePosition.OUTSIDE)
    }

    for (let row = 0; row < expandedMap.length; ++row) {
        for (let col = 0; col < expandedMap[row].length; ++col) {
            const t = expandedMap[row][col]
            if (t.position === undefined) t.position = TilePosition.INSIDE
        }
    }

    return map.flatMap(r => r.filter(t => t.position === TilePosition.INSIDE)).length
}

function walkMap(map: TileMap) {
    const [startRow, startCol] = findStart(map)
    map[startRow][startCol].distance = 0

    const queue: number[][] = [[startRow, startCol, 0]]
    while (queue.length > 0) {
        const [row, col, distance] = queue.shift() as number[]
        const tile = map[row][col]

        if (tile.type == TileType.START) {
            const northTile = mapGet(map, row - 1, col)
            const southTile = mapGet(map, row + 1, col)
            const westTile = mapGet(map, row, col - 1)
            const eastTile = mapGet(map, row, col + 1)

            let north = northTile !== undefined && paths(northTile.type).some(([dr,]) => dr === 1)
            let south = southTile !== undefined && paths(southTile.type).some(([dr,]) => dr === -1)
            let west = westTile !== undefined && paths(westTile.type).some(([, dc]) => dc === 1)
            let east = eastTile !== undefined && paths(eastTile.type).some(([, dc]) => dc === -1)

            if (north && south) tile.type = TileType.VERTICAL
            else if (west && east) tile.type = TileType.HORIZONTAL
            else if (north && west) tile.type = TileType.NORTH_WEST
            else if (north && east) tile.type = TileType.NORTH_EAST
            else if (south && west) tile.type = TileType.SOUTH_WEST
            else if (south && east) tile.type = TileType.SOUTH_EAST
            else throw new Error("Can't deduce start tile")
        }

        for (const [dr, dc] of paths(tile.type)) {
            testTile(row + dr, col + dc, distance + 1)
        }
    }

    return map

    function testTile(row: number, col: number, distance: number) {
        const tile = map[row][col]
        if (tile !== undefined && tile.type !== TileType.GROUND && (Number.isNaN(tile.distance) || distance < tile.distance)) {
            tile.distance = distance
            queue.push([row, col, distance])
        }
    }
}

function mapGet(map: TileMap, row: number, col: number) {
    const tileRow = map[row]
    if (tileRow === undefined) return undefined
    return tileRow[col]
}

function findStart(map: TileMap) {
    for (let r = 0; r < map.length; ++r) {
        const row = map[r]
        for (let c = 0; c < row.length; ++c) {
            if (row[c].type === TileType.START) return [r, c]
        }
    }

    throw new Error("No start position")
}

function paths(type: TileType) {
    switch (type) {
        case TileType.START:
            return []
        case TileType.VERTICAL:
            return [[-1, 0], [1, 0]]
        case TileType.HORIZONTAL:
            return [[0, -1], [0, 1]]
        case TileType.NORTH_EAST:
            return [[-1, 0], [0, 1]]
        case TileType.NORTH_WEST:
            return [[-1, 0], [0, -1]]
        case TileType.SOUTH_WEST:
            return [[1, 0], [0, -1]]
        case TileType.SOUTH_EAST:
            return [[1, 0], [0, 1]]
        case TileType.GROUND:
            return []
        default:
            throw new Error("Unknown tile")
    }
}

function expandMap(map: TileMap) {
    const newMap: TileMap = []
    for (const tileRow of map) {
        const newRow: Tile[] = []
        for (const tile of tileRow) {
            newRow.push(tile)
            if (Number.isNaN(tile.distance)) {
                newRow.push({
                    type: TileType.GROUND,
                    distance: Number.NaN,
                    position: undefined,
                })
            } else if (paths(tile.type).some(([, dc]) => dc === 1)) {
                newRow.push({
                    type: TileType.HORIZONTAL,
                    distance: tile.distance,
                    position: TilePosition.PIPE,
                })
            } else {
                newRow.push({
                    type: TileType.GROUND,
                    distance: Number.NaN,
                    position: undefined,
                })
            }
        }
        newMap.push(newRow)

        const nextRow: Tile[] = []
        for (const tile of newRow) {
            if (Number.isNaN(tile.distance)) {
                nextRow.push({
                    type: TileType.GROUND,
                    distance: Number.NaN,
                    position: undefined,
                })
            } else if (paths(tile.type).some(([dr,]) => dr === 1)) {
                nextRow.push({
                    type: TileType.VERTICAL,
                    distance: tile.distance,
                    position: TilePosition.PIPE,
                })
            } else {
                nextRow.push({
                    type: TileType.GROUND,
                    distance: Number.NaN,
                    position: undefined,
                })
            }
        }
        newMap.push(nextRow)
    }

    return newMap
}

function floodFill(map: TileMap, row: number, col: number, position: TilePosition) {
    const queue: number[][] = []
    enqueue(row, col)

    while (queue.length > 0) {
        const [r, c] = queue.shift() as number[]
        const tile = map[r][c]
        if (tile === undefined) continue

        testTile(r, c, -1, 0)
        testTile(r, c, 1, 0)
        testTile(r, c, 0, 1)
        testTile(r, c, 0, -1)
    }

    function testTile(r: number, c: number, dr: number, dc: number) {
        assert(dr === 0 || dc === 0)
        assert(dr !== 0 || dc !== 0)

        const t = mapGet(map, r + dr, c + dc)
        if (t === undefined) return

        if (t.position === undefined) {
            enqueue(r + dr, c + dc)
        }
    }

    function enqueue(r: number, c: number) {
        const t = map[r][c]
        if (t.position !== undefined) return
        if (Number.isNaN(t.distance)) t.position = position
        else t.position = TilePosition.PIPE

        queue.push([r, c])
    }
}

function parseMapFromFile(path: string) {
    const reader = createReadStream(path)
    const input = readline.createInterface(reader)
    return parseMap(input)
}

async function parseMap(input: readline.Interface) {
    const map: TileMap = []
    for await (const line of input) {
        const tiles = Array.from(line)
            .map(toTileType)
            .map(type => ({ type, distance: Number.NaN, position: undefined }))
        map.push(tiles)
    }

    return map
}

function toTileType(ch: string) {
    assert.strictEqual(ch.length, 1)

    switch (ch) {
        case '|': return TileType.VERTICAL
        case '-': return TileType.HORIZONTAL
        case 'L': return TileType.NORTH_EAST
        case 'J': return TileType.NORTH_WEST
        case '7': return TileType.SOUTH_WEST
        case 'F': return TileType.SOUTH_EAST
        case '.': return TileType.GROUND
        case 'S': return TileType.START
        default: throw new Error("Parse error")
    }
}

function printMap(map: TileMap) {
    const text = map.map(r =>
        r.map(t =>
            fmtSymbol(t)
        ).join('')
    ).join('\n')

    console.log(text)
}

function printMapDist(map: TileMap, pad: number) {
    const text = map.map(r =>
        r.map(t =>
            fmt(t.distance).padStart(3)
        ).join('')
    ).join('\n')

    console.log(text)
}

function printMapPos(map: TileMap) {
    const text = map.map(r =>
        r.map(t =>
            fmtPosition(t)
        ).join('')
    ).join('\n')

    console.log(text)
}

function fmt(x: number) {
    if (Number.isFinite(x)) return x.toString()
    return '.'
}

function fmtSymbol(tile: Tile) {
    if (Number.isNaN(tile.distance)) return ' '

    switch (tile.type) {
        case TileType.START: return '*'
        case TileType.VERTICAL: return '|'
        case TileType.HORIZONTAL: return '-'
        case TileType.NORTH_EAST: return '⌞'
        case TileType.NORTH_WEST: return '⌟'
        case TileType.SOUTH_WEST: return '⌝'
        case TileType.SOUTH_EAST: return '⌜'
        case TileType.GROUND: return '.'
        default:
            throw new Error("Unknown tile")
    }
}

function fmtPosition(tile: Tile) {
    switch (tile.position) {
        case TilePosition.INSIDE: return 'I'
        case TilePosition.OUTSIDE: return 'O'
        case TilePosition.PIPE: return '-'
        case undefined: return '.'
        default: throw new Error("Unknown position")
    }
}
