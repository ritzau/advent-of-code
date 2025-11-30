/** AoC 2023 Day 12: Hot Springs */

type SpringPattern = {
  template: string[];
  runLengths: number[];
};

enum MatchResult {
  YES,
  MAYBE,
  IMPOSSIBLE,
}

function parsePatterns(input: string): SpringPattern[] {
  const lines = input.split("\n");
  const patterns: SpringPattern[] = [];
  for (const line of lines) {
    if (line.trim().length === 0) continue;
    const m = line.match(/([?.#]+)\s+(\d+(?:,\d+)*)/);
    if (m === null) throw new Error(`Parse error: ${line}`);
    patterns.push({
      template: Array.from(m[1]),
      runLengths: m[2].split(",").map((x) => parseInt(x)),
    });
  }

  return patterns;
}

type Combo3State = {
  template: string[];
  runLengths: number[];
  runLengthCount: number;
};

function simplify(template: string[]): string[] {
  let s = template.join("");
  s = s.replace(/\.+$/, "");
  s = s.replace(/\.+/g, ".");
  return Array.from(s);
}

const combo3Memo = new Map<string, number>();

function combo3(state: Combo3State, prefix = ""): number {
  state.template = simplify(state.template);

  const memoKey = JSON.stringify(state);
  const memoizedResult = combo3Memo.get(memoKey);
  if (memoizedResult !== undefined) return memoizedResult;

  let { template, runLengths, runLengthCount } = state;

  let position = 0;
  let runLengthIndex = 0;

  while (position < template.length) {
    const ch = template[position];
    if (runLengthCount > 0) {
      if (runLengthCount < runLengths[runLengthIndex]) {
        if (ch === "?") {
          template[position] = "#";
          runLengthCount++;
        } else if (ch === "#") {
          runLengthCount++;
        } else if (ch === ".") {
          combo3Memo.set(memoKey, 0);
          return 0;
        }
      } else if (runLengthCount === runLengths[runLengthIndex]) {
        if (ch === "?") {
          template[position] = ".";
          runLengthCount = 0;
          ++runLengthIndex;
        } else if (ch === "#") {
          combo3Memo.set(memoKey, 0);
          return 0;
        } else if (ch === ".") {
          runLengthCount = 0;
          ++runLengthIndex;
        }
      } else {
        combo3Memo.set(memoKey, 0);
        return 0;
      }
    } else if (runLengthCount === 0) {
      if (ch === "?") {
        if (runLengthIndex >= runLengths.length) {
          template[position] = ".";
        } else {
          const result =
            combo3(
              {
                template: template.slice(position + 1),
                runLengthCount: 1,
                runLengths: runLengths.slice(runLengthIndex),
              },
              prefix + template.slice(0, position).join("") + "#"
            ) +
            combo3(
              {
                template: template.slice(position + 1),
                runLengthCount: 0,
                runLengths: runLengths.slice(runLengthIndex),
              },
              prefix + template.slice(0, position).join("") + "."
            );

          combo3Memo.set(memoKey, result);
          return result;
        }
      } else if (ch === "#") {
        runLengthCount = 1;
      } else if (ch === ".") {
        // all good
      }
    }
    ++position;
  }

  const isAtEnd = position === template.length;
  const hasSeenAllGroups =
    runLengthIndex === runLengths.length && runLengthCount === 0;
  const isInLastGroupAndSeenAllHashes =
    runLengthIndex === runLengths.length - 1 &&
    runLengthCount === runLengths[runLengthIndex];

  if (isAtEnd && (hasSeenAllGroups || isInLastGroupAndSeenAllHashes)) {
    combo3Memo.set(memoKey, 1);
    return 1;
  } else {
    combo3Memo.set(memoKey, 0);
    return 0;
  }
}

export function solvePart1(input: string): number {
  const patterns = parsePatterns(input);

  let count = 0;
  for (const pattern of patterns) {
    const c = combo3({
      template: Array.from(pattern.template),
      runLengthCount: 0,
      runLengths: pattern.runLengths,
    });
    count += c;
  }

  return count;
}

export function solvePart2(input: string): number {
  // Part 2 is commented out in the original as it's too slow
  // Return 0 for now
  return 0;
}
