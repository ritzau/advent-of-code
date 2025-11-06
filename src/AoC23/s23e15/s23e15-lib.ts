/** AoC 2023 Day 15: Lens Library */

type Lens = { label: string; focal: number };
type Boxes = Lens[][];

function hash(s: string): number {
  return Array.from(s).reduce((hash, ch) => {
    return (17 * (hash + ch.charCodeAt(0))) % 256;
  }, 0);
}

function execute(boxes: Boxes, operation: string) {
  const m = operation.match(/(\w+)([=-])(\d*)/);
  if (m === null) {
    throw new Error(`Parse error: ${operation}`);
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
      if (index !== -1) {
        box[index] = { label, focal: parseInt(value) };
      } else {
        box.push({ label, focal: parseInt(value) });
      }
      break;
  }
}

function focusPower(id: number, box: Lens[]): number {
  return box.reduce((p, { focal: f }, i) => p + (id + 1) * (i + 1) * f, 0);
}

export function solvePart1(input: string): number {
  return input
    .split(",")
    .reduce((a, b) => a + hash(b), 0);
}

export function solvePart2(input: string): number {
  const boxes: Boxes = Array.from({ length: 256 }, () => []);
  input
    .split(",")
    .forEach((op) => execute(boxes, op));
  return boxes.map((b, s) => focusPower(s, b)).reduce((a, b) => a + b);
}
