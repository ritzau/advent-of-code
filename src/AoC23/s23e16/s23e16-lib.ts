/** AoC 2023 Day 16: The Floor Will Be Lava */

function parseMap(input: string): string[][] {
  return input
    .split("\n")
    .filter((l) => l.length > 0)
    .map((l) => Array.from(l));
}

enum Direction {
  UP,
  DOWN,
  LEFT,
  RIGHT,
}

type Step = { r: number; c: number; d: Direction };

function stepBeam(map: string[][], startStep: Step): number {
  const visited = new Set<number>();
  const queue = [startStep];

  while (true) {
    const step = queue.shift();
    if (step === undefined) break;

    const { r, c, d } = step;
    if (r < 0 || map.length <= r) continue;
    if (c < 0 || map[r].length <= c) continue;

    const stepId = r * 1000000 + c * 1000 + d;
    if (visited.has(stepId)) continue;
    visited.add(stepId);

    const tile = map[r][c];
    switch (tile) {
      case "^":
      case "v":
      case "<":
      case ">":
      case ".":
        queue.push(next(step, d));
        break;
      case "/":
        queue.push(next(step, slashReflection(d)));
        break;
      case "\\":
        queue.push(next(step, backslashReflection(d)));
        break;
      case "|":
        if (d == Direction.LEFT || d == Direction.RIGHT) {
          queue.push(next(step, Direction.UP));
          queue.push(next(step, Direction.DOWN));
        } else {
          queue.push(next(step, d));
        }
        break;
      case "-":
        if (d == Direction.UP || d == Direction.DOWN) {
          queue.push(next(step, Direction.LEFT));
          queue.push(next(step, Direction.RIGHT));
        } else {
          queue.push(next(step, d));
        }
        break;
    }
  }

  return new Set(Array.from(visited).map((id) => Math.trunc(id / 1000))).size;

  function slashReflection(d: Direction): Direction {
    switch (d) {
      case Direction.UP: return Direction.RIGHT;
      case Direction.DOWN: return Direction.LEFT;
      case Direction.LEFT: return Direction.DOWN;
      case Direction.RIGHT: return Direction.UP;
    }
  }

  function backslashReflection(d: Direction): Direction {
    switch (d) {
      case Direction.UP: return Direction.LEFT;
      case Direction.DOWN: return Direction.RIGHT;
      case Direction.LEFT: return Direction.UP;
      case Direction.RIGHT: return Direction.DOWN;
    }
  }

  function next(step: Step, direction: Direction): Step {
    const { dr, dc } = deltas(direction);
    return { r: step.r + dr, c: step.c + dc, d: direction };
  }

  function deltas(d: Direction) {
    switch (d) {
      case Direction.UP: return { dr: -1, dc: 0 };
      case Direction.DOWN: return { dr: 1, dc: 0 };
      case Direction.LEFT: return { dr: 0, dc: -1 };
      case Direction.RIGHT: return { dr: 0, dc: 1 };
    }
  }
}

export function solvePart1(input: string): number {
  const map = parseMap(input);
  return stepBeam(map, { r: 0, c: 0, d: Direction.RIGHT });
}

export function solvePart2(input: string): number {
  const map = parseMap(input);
  let max = 0;

  for (let r = 0; r < map.length; ++r) {
    const energizedLeft = stepBeam(map, { r: r, c: 0, d: Direction.RIGHT });
    const energizedRight = stepBeam(map, {
      r: r,
      c: map[r].length - 1,
      d: Direction.LEFT,
    });
    max = Math.max(max, energizedLeft, energizedRight);
  }

  for (let c = 0; c < map[0].length; ++c) {
    const energizedTop = stepBeam(map, { r: 0, c: c, d: Direction.DOWN });
    const energizedBottom = stepBeam(map, {
      r: map.length - 1,
      c: c,
      d: Direction.UP,
    });
    max = Math.max(max, energizedTop, energizedBottom);
  }

  return max;
}
