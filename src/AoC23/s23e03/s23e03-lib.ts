/** AoC 2023 Day 3: Gear Ratios */

type Number = {
  startIndex: number;
  lastIndex: number;
  value: string;
  id: number;
};

type SymbolPredicate = (x: string) => boolean;

type NeighbourHandler = {
  (ns: Number[]): Generator<number, void, unknown>;
  (ns: Number[]): Generator<number, void, unknown>;
};

class EngineSchematic {
  private lines: string[][] = [];
  private numbers: Number[][] = [];
  private nextId = 0;

  private isSymbol: SymbolPredicate;
  private handleNeighbours: NeighbourHandler;

  constructor(isSymbol: SymbolPredicate, handleNeighbours: NeighbourHandler) {
    this.isSymbol = isSymbol;
    this.handleNeighbours = handleNeighbours;
  }

  *addLine(line: string) {
    while (this.lines.length > 2) {
      this.lines.shift();
      this.numbers.shift();
    }

    if (this.lines.length === 0) {
      this.lines.push(Array.from(".".repeat(line.length)));
      this.numbers.push([]);
    }

    const lineArray = Array.from(line);
    this.lines.push(lineArray);
    this.numbers.push(this.parseNumbers(lineArray));

    if (this.lines.length >= 3) {
      const symbolIndices = Array.from(this.lines[1]).flatMap((c, i) =>
        this.isSymbol(c) ? i : [],
      );
      for (const g of symbolIndices) {
        yield* this.findNeighbours(g);
      }
    }
  }

  *addLastLine() {
    yield* this.addLine(".".repeat(this.lines[0].length));
  }

  private *findNeighbours(g: number) {
    const neighbourNumbers = new Map<number, Number>();
    for (let r = 0; r <= 2; ++r) {
      for (let c = -1; c <= 1; ++c) {
        if (r === 1 && c === 0) continue;
        const n = this.findNumber(r, g + c);
        if (n !== undefined) neighbourNumbers.set(n.id, n);
      }
    }

    yield* this.handleNeighbours(Array.from(neighbourNumbers.values()));
  }

  private findNumber(row: number, col: number) {
    return this.numbers[row].find(
      (n) => n.startIndex <= col && col <= n.lastIndex,
    );
  }

  private parseNumbers(line: string[]): Number[] {
    let isNumber = false;
    const ns: Number[] = [];
    let n: Number = {
      startIndex: -1,
      lastIndex: -1,
      value: "",
      id: -1,
    };

    for (const [i, c] of line.entries()) {
      if (isNumber) {
        if (isDigit(c)) {
          n.value += c;
          n.lastIndex = i;
        } else {
          isNumber = false;
          n = {
            startIndex: -1,
            lastIndex: -1,
            value: "",
            id: -1,
          };
        }
      } else {
        if (isDigit(c)) {
          ns.push(n);
          n.value += c;
          n.startIndex = i;
          n.lastIndex = i;
          n.id = this.nextId++;
          isNumber = true;
        }
      }
    }

    return ns;
  }
}

function isSymbol(x: string): boolean {
  return x.match(/[^\d.]/) !== null;
}

function isDigit(x: string) {
  return x.match(/\d/);
}

function* partNumbers(lines: string[]) {
  function* handleNeighours(ns: Number[]) {
    yield* ns.map((n) => parseInt(n.value));
  }

  const schematics = new EngineSchematic((c) => isSymbol(c), handleNeighours);

  for (const line of lines) {
    if (line.length === 0) continue;
    yield* schematics.addLine(line);
  }
  yield* schematics.addLastLine();
}

function* gearRatios(lines: string[]) {
  function* handleNeighours(ns: Number[]) {
    if (ns.length === 2) {
      yield ns.map((n) => parseInt(n.value)).reduce((a, b) => a * b);
    }
  }

  const schematics = new EngineSchematic((x) => x === "*", handleNeighours);

  for (const line of lines) {
    if (line.length === 0) continue;
    yield* schematics.addLine(line);
  }
  yield* schematics.addLastLine();
}

export function solvePart1(input: string): number {
  const lines = input.split("\n");
  return Array.from(partNumbers(lines)).reduce((a, b) => a + b, 0);
}

export function solvePart2(input: string): number {
  const lines = input.split("\n");
  return Array.from(gearRatios(lines)).reduce((a, b) => a + b, 0);
}
