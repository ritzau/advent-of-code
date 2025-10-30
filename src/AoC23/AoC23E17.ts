import { readFileSync } from "fs";
import { logResult } from "./aoclib";
import assert from "node:assert";

export async function main(all: boolean) {
  logResult("Part 1 - sample", part1("AoC23E17-sample.txt"), 102);
  if (all) logResult("Part 1 - input", part1("AoC23E17-input.txt"), 916);
  logResult("Part 2 - sample", part2("AoC23E17-sample.txt"), 94);
  if (all) logResult("Part 2 - input", part2("AoC23E17-input.txt"), 1067);
}

type Node = {
  distance: number;
  visited: boolean;
  x: number;
  y: number;
  direction: Direction;
  straight: number;
  from: Node | undefined;
};

type Tile = {
  weight: number;
  nodes: Node[][];
  direction: string;
};

type Map = Tile[][];

enum Direction {
  NORTH,
  SOUTH,
  WEST,
  EAST,
}

function part1(path: string) {
  const map = parseMap(path);
  return walk(map, 1, 3);
}

function part2(path: string) {
  const map = parseMap(path);
  return walk(map, 4, 10);
}

function parseMap(path: string): Map {
  return readFileSync(path)
    .toString()
    .split("\n")
    .filter((l) => l.length > 0)
    .map((l, y) =>
      Array.from(l).map((c, x) => {
        return {
          weight: parseInt(c),
          direction: "",
          nodes: [] as Node[][],
        };
      }),
    );
}

function walk(map: Map, minStraight: number, maxStraight: number) {
  let queue: Node[] = [getNode(0, 0, Direction.EAST, 1)];
  queue[0].distance = 0;

  while (queue.length > 0) {
    const node = nextNode();
    assert.equal(node.visited, false);

    if (node.straight >= minStraight) {
      move(node, turnLeft(node), 1);
      move(node, turnRight(node), 1);
    }
    if (node.straight < maxStraight) {
      move(node, node.direction, node.straight + 1);
    }

    node.visited = true;
    queue = queue.filter(({ visited }) => !visited);
  }

  const ns = map[map.length - 1][map[0].length - 1].nodes
    .flat()
    .filter((n) => n.straight >= minStraight)
    .map((n) => n.distance);

  return Math.min(...ns);

  function nextNode() {
    let node = queue[0];
    for (const n of queue) {
      if (n.distance < node.distance) {
        node = n;
      }
    }
    return node;
  }

  function turnLeft(n: Node) {
    switch (n.direction) {
      case Direction.NORTH:
        return Direction.WEST;
      case Direction.SOUTH:
        return Direction.EAST;
      case Direction.WEST:
        return Direction.SOUTH;
      case Direction.EAST:
        return Direction.NORTH;
    }
  }

  function turnRight(n: Node) {
    switch (n.direction) {
      case Direction.NORTH:
        return Direction.EAST;
      case Direction.SOUTH:
        return Direction.WEST;
      case Direction.WEST:
        return Direction.NORTH;
      case Direction.EAST:
        return Direction.SOUTH;
    }
  }

  function move(from: Node, direction: Direction, straight: number) {
    const { dx, dy } = delta(direction);
    const toX = from.x + dx;
    const toY = from.y + dy;
    if (toX < 0 || map[0].length <= toX) {
      return;
    }
    if (toY < 0 || map.length <= toY) {
      return;
    }

    const tile = map[toY][toX];
    const to = getNode(toX, toY, direction, straight);
    if (to.visited) {
      return;
    }

    const previousDistance = from.distance;
    const potentialDistance = previousDistance + tile.weight;
    if (potentialDistance < to.distance) {
      to.distance = previousDistance + tile.weight;
      to.straight = straight;
      to.from = from;
      queue.push(to);
    }
  }

  function delta(direction: Direction) {
    switch (direction) {
      case Direction.NORTH:
        return { dx: 0, dy: -1 };
      case Direction.SOUTH:
        return { dx: 0, dy: 1 };
      case Direction.WEST:
        return { dx: -1, dy: 0 };
      case Direction.EAST:
        return { dx: 1, dy: 0 };
    }
  }

  function getNode(
    x: number,
    y: number,
    direction: Direction,
    straight: number,
  ) {
    const tile = map[y][x];
    let ns = tile.nodes[direction];
    if (ns === undefined) {
      ns = [];
      tile.nodes[direction] = ns;
    }

    let n = ns[straight];
    if (n === undefined) {
      n = createNode(x, y, direction, straight, Infinity);
      ns[straight] = n;
    }

    return n;
  }
}

function createNode(
  x: number,
  y: number,
  direction: Direction,
  straight: number,
  distance: number,
) {
  return {
    distance,
    visited: false,
    from: undefined,
    direction,
    straight,
    x,
    y,
  };
}

function tracePath(map: Map, node: Node) {
  if (node.from === undefined) return;
  tracePath(map, node.from);

  let direction: string = "?";
  if (node.from.x < node.x) direction = ">";
  if (node.from.x > node.x) direction = "<";
  if (node.from.y > node.y) direction = "^";
  if (node.from.y < node.y) direction = "v";

  map[node.y][node.x].direction = direction;
}

function printMap(map: Map) {
  console.log(
    map
      .map((l, y) =>
        l
          .map((t, x) => {
            if (t.direction.trim().length > 0) return t.direction;
            else return t.weight.toString();
          })
          .join(""),
      )
      .join("\n"),
  );
}
