/** AoC 2023 Day 10: Pipe Maze */

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
  type: TileType;
  distance: number;
  position: TilePosition | undefined;
};

type TileMap = Tile[][];

function parseMap(input: string): TileMap {
  const lines = input.split("\n");
  const map: TileMap = [];
  for (const line of lines) {
    if (line.trim().length === 0) continue;
    const tiles = Array.from(line)
      .map(toTileType)
      .map((type) => ({ type, distance: Number.NaN, position: undefined }));
    map.push(tiles);
  }

  return map;
}

function toTileType(ch: string): TileType {
  switch (ch) {
    case "|": return TileType.VERTICAL;
    case "-": return TileType.HORIZONTAL;
    case "L": return TileType.NORTH_EAST;
    case "J": return TileType.NORTH_WEST;
    case "7": return TileType.SOUTH_WEST;
    case "F": return TileType.SOUTH_EAST;
    case ".": return TileType.GROUND;
    case "S": return TileType.START;
    default: throw new Error("Parse error");
  }
}

function paths(type: TileType): number[][] {
  switch (type) {
    case TileType.START: return [];
    case TileType.VERTICAL: return [[-1, 0], [1, 0]];
    case TileType.HORIZONTAL: return [[0, -1], [0, 1]];
    case TileType.NORTH_EAST: return [[-1, 0], [0, 1]];
    case TileType.NORTH_WEST: return [[-1, 0], [0, -1]];
    case TileType.SOUTH_WEST: return [[1, 0], [0, -1]];
    case TileType.SOUTH_EAST: return [[1, 0], [0, 1]];
    case TileType.GROUND: return [];
    default: throw new Error("Unknown tile");
  }
}

function mapGet(map: TileMap, row: number, col: number): Tile | undefined {
  const tileRow = map[row];
  if (tileRow === undefined) return undefined;
  return tileRow[col];
}

function findStart(map: TileMap): [number, number] {
  for (let r = 0; r < map.length; ++r) {
    const row = map[r];
    for (let c = 0; c < row.length; ++c) {
      if (row[c].type === TileType.START) return [r, c];
    }
  }

  throw new Error("No start position");
}

function walkMap(map: TileMap): TileMap {
  const [startRow, startCol] = findStart(map);
  map[startRow][startCol].distance = 0;

  const queue: number[][] = [[startRow, startCol, 0]];
  while (queue.length > 0) {
    const [row, col, distance] = queue.shift() as number[];
    const tile = map[row][col];

    if (tile.type == TileType.START) {
      const northTile = mapGet(map, row - 1, col);
      const southTile = mapGet(map, row + 1, col);
      const westTile = mapGet(map, row, col - 1);
      const eastTile = mapGet(map, row, col + 1);

      let north = northTile !== undefined && paths(northTile.type).some(([dr]) => dr === 1);
      let south = southTile !== undefined && paths(southTile.type).some(([dr]) => dr === -1);
      let west = westTile !== undefined && paths(westTile.type).some(([, dc]) => dc === 1);
      let east = eastTile !== undefined && paths(eastTile.type).some(([, dc]) => dc === -1);

      if (north && south) tile.type = TileType.VERTICAL;
      else if (west && east) tile.type = TileType.HORIZONTAL;
      else if (north && west) tile.type = TileType.NORTH_WEST;
      else if (north && east) tile.type = TileType.NORTH_EAST;
      else if (south && west) tile.type = TileType.SOUTH_WEST;
      else if (south && east) tile.type = TileType.SOUTH_EAST;
      else throw new Error("Can't deduce start tile");
    }

    for (const [dr, dc] of paths(tile.type)) {
      testTile(row + dr, col + dc, distance + 1);
    }
  }

  return map;

  function testTile(row: number, col: number, distance: number) {
    const tile = map[row][col];
    if (
      tile !== undefined &&
      tile.type !== TileType.GROUND &&
      (Number.isNaN(tile.distance) || distance < tile.distance)
    ) {
      tile.distance = distance;
      queue.push([row, col, distance]);
    }
  }
}

function expandMap(map: TileMap): TileMap {
  const newMap: TileMap = [];
  for (const tileRow of map) {
    const newRow: Tile[] = [];
    for (const tile of tileRow) {
      newRow.push(tile);
      if (Number.isNaN(tile.distance)) {
        newRow.push({ type: TileType.GROUND, distance: Number.NaN, position: undefined });
      } else if (paths(tile.type).some(([, dc]) => dc === 1)) {
        newRow.push({ type: TileType.HORIZONTAL, distance: tile.distance, position: TilePosition.PIPE });
      } else {
        newRow.push({ type: TileType.GROUND, distance: Number.NaN, position: undefined });
      }
    }
    newMap.push(newRow);

    const nextRow: Tile[] = [];
    for (const tile of newRow) {
      if (Number.isNaN(tile.distance)) {
        nextRow.push({ type: TileType.GROUND, distance: Number.NaN, position: undefined });
      } else if (paths(tile.type).some(([dr]) => dr === 1)) {
        nextRow.push({ type: TileType.VERTICAL, distance: tile.distance, position: TilePosition.PIPE });
      } else {
        nextRow.push({ type: TileType.GROUND, distance: Number.NaN, position: undefined });
      }
    }
    newMap.push(nextRow);
  }

  return newMap;
}

function floodFill(map: TileMap, row: number, col: number, position: TilePosition) {
  const queue: number[][] = [];
  enqueue(row, col);

  while (queue.length > 0) {
    const [r, c] = queue.shift() as number[];
    const tile = map[r][c];
    if (tile === undefined) continue;

    testTile(r, c, -1, 0);
    testTile(r, c, 1, 0);
    testTile(r, c, 0, 1);
    testTile(r, c, 0, -1);
  }

  function testTile(r: number, c: number, dr: number, dc: number) {
    const t = mapGet(map, r + dr, c + dc);
    if (t === undefined) return;

    if (t.position === undefined) {
      enqueue(r + dr, c + dc);
    }
  }

  function enqueue(r: number, c: number) {
    const t = map[r][c];
    if (t.position !== undefined) return;
    if (Number.isNaN(t.distance)) t.position = position;
    else t.position = TilePosition.PIPE;

    queue.push([r, c]);
  }
}

export function solvePart1(input: string): number {
  const map = walkMap(parseMap(input));
  return Math.max(
    ...map.flatMap((r) =>
      r.map((t) => t.distance).filter((x) => !Number.isNaN(x)),
    ),
  );
}

export function solvePart2(input: string): number {
  const map = walkMap(parseMap(input));

  for (let row = 0; row < map.length; ++row) {
    for (let col = 0; col < map[row].length; ++col) {
      const t = map[row][col];
      if (!Number.isNaN(t.distance)) t.position = TilePosition.PIPE;
    }
  }

  const expandedMap = expandMap(map);

  for (let row = 0; row < expandedMap.length; ++row) {
    floodFill(expandedMap, row, 0, TilePosition.OUTSIDE);
    floodFill(expandedMap, row, expandedMap[row].length - 1, TilePosition.OUTSIDE);
  }

  for (let i = 0; i < expandedMap[0].length; ++i) {
    floodFill(expandedMap, 0, i, TilePosition.OUTSIDE);
    floodFill(expandedMap, expandedMap.length - 1, i, TilePosition.OUTSIDE);
  }

  for (let row = 0; row < expandedMap.length; ++row) {
    for (let col = 0; col < expandedMap[row].length; ++col) {
      const t = expandedMap[row][col];
      if (t.position === undefined) t.position = TilePosition.INSIDE;
    }
  }

  return map.flatMap((r) => r.filter((t) => t.position === TilePosition.INSIDE)).length;
}
