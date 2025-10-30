import { readFileSync } from "fs";
import { logResult } from "./aoclib";
import assert from "assert";

export async function main() {
  logResult("Part 1 - sample", await part1("AoC23E15-sample.txt"), 1320);
  logResult("Part 1 - input", await part1("AoC23E15-input.txt"), 510013);
  logResult("Part 2 - sample", await part2("AoC23E15-sample.txt"), 145);
  logResult("Part 2 - input", await part2("AoC23E15-input.txt"), 268497);
}

type Lens = { label: string; focal: number };
type Boxes = Lens[][];

async function part1(path: string) {
  return readFileSync(path)
    .toString()
    .trim()
    .split(",")
    .reduce((a, b) => a + hash(b), 0);
}

async function part2(path: string) {
  const boxes: Boxes = Array.from({ length: 256 }, () => []);
  readFileSync(path)
    .toString()
    .trim()
    .split(",")
    .forEach((op) => execute(boxes, op));
  return boxes.map((b, s) => focusPower(s, b)).reduce((a, b) => a + b);
}

function hash(s: string) {
  return Array.from(s).reduce((hash, ch) => {
    return (17 * (hash + ch.charCodeAt(0))) % 256;
  }, 0);
}

function execute(boxes: Boxes, operation: string) {
  const m = operation.match(/(\w+)([=-])(\d*)/);
  if (m === null) {
    assert.fail();
  }
  const [, label, op, value] = m;
  const box = boxes[hash(label)];
  const index = box.findIndex(({ label: l }) => label === l);
  switch (op) {
    case "-":
      if (index !== -1) {
        box.splice(index, 1);
      }
      break;
    case "=":
      assert.notStrictEqual(value, undefined);
      if (index !== -1) {
        box[index] = { label, focal: parseInt(value) };
      } else {
        box.push({ label, focal: parseInt(value) });
      }
      break;
    default:
      assert.fail();
  }
}

function focusPower(id: number, box: Lens[]) {
  return box.reduce((p, { focal: f }, i) => p + (id + 1) * (i + 1) * f, 0);
}
