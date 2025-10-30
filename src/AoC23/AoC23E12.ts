import * as readline from "node:readline";

import { createReadStream } from "fs";
import { logResult } from "./aoclib";
import assert from "node:assert";

export async function main() {
  logResult("Part 1a - sample", await part1a("AoC23E12-sample.txt"), 21);
  logResult("Part 1b - sample", await part1b("AoC23E12-sample.txt"), 21);
  logResult("Part 1a - input", await part1a("AoC23E12-input.txt"), 7204);
  logResult("Part 1b - input", await part1b("AoC23E12-input.txt"), 7204);
  // logResult("Part 2 - sample", await part2('AoC23E12-sample.txt'), 525152)
  // logResult("Part 2 - input", await part2('AoC23E12-input.txt'), 7204)
}

type SpringPattern = {
  template: string[];
  runLengths: number[];
};

async function part1a(path: string) {
  const readable = createReadStream(path);
  const input = readline.createInterface(readable);
  const patterns = await parsePatterns(input);

  let count = 0;
  let patternIndex = 0;
  for (const pattern of patterns) {
    // console.group(patternIndex++, pattern.template.join(''), pattern.runLengths)
    let c = 0;
    for (const combination of combinations(
      pattern.template,
      pattern.runLengths,
    )) {
      // console.log(combination)
      ++c;
    }
    // console.log(pattern.template.join(''), "=>", c)
    count += c;
    // console.groupEnd()
  }

  return count;
}

async function part1b(path: string) {
  const readable = createReadStream(path);
  const input = readline.createInterface(readable);
  const patterns = await parsePatterns(input);

  let count = 0;
  let patternIndex = 0;
  for (const pattern of patterns) {
    // console.group(patternIndex++, pattern.template.join(''), pattern.runLengths)
    const c = combo3({
      template: Array.from(pattern.template),
      runLengthCount: 0,
      runLengths: pattern.runLengths,
    });
    // console.log(pattern.template.join(''), "=>", c)
    count += c;
    // console.groupEnd()
  }

  return count;
}

async function part2(path: string) {
  const readable = createReadStream(path);
  const input = readline.createInterface(readable);
  const patterns = await parsePatterns(input);

  let count = 0;
  for (const { template, runLengths } of patterns) {
    const t = template.join("");
    const t2 = `${t}?${t}?${t}?${t}?${t}`;
    const rl2 = runLengths.concat(
      runLengths,
      runLengths,
      runLengths,
      runLengths,
    );
    console.log(t);
    const c = combo3({
      template: Array.from(t2),
      runLengthCount: 0,
      runLengths: rl2,
    });
    // console.log(t, "=>", c)
    count += c;
    // break
  }

  return count;
}

async function parsePatterns(input: readline.Interface) {
  const patterns: SpringPattern[] = [];
  for await (const line of input) {
    const m = line.match(/([?.#]+)\s+(\d+(?:,\d+)*)/);
    if (m === null) throw new Error(`Parse error: ${line}`);
    patterns.push({
      template: Array.from(m[1]),
      runLengths: m[2].split(",").map((x) => parseInt(x)),
    });
  }

  return patterns;
}

function* combinations(template: string[], runLengths: number[]) {
  let count = 0;
  const totalHashes = runLengths.reduce((a, b) => a + b);

  const queue: [template: string[], count: number][] = [[template, 0]];
  while (true) {
    if (++count > 1000) break;
    const item = queue.shift();
    if (item === undefined) break;
    const [t, hashCount] = item;

    const index = t.indexOf("?");
    if (index === -1) {
      if (isMatching(t, runLengths) === MatchResult.YES) {
        yield t;
      }
    } else if (
      t.filter((ch) => ch === "?" || ch === "#").length < totalHashes
    ) {
      // Ignore
    } else {
      const withDot = Array.from(t);
      withDot[index] = ".";

      if (isMatching(withDot, runLengths) != MatchResult.IMPOSSIBLE) {
        queue.push([withDot, hashCount]);
      }

      if (hashCount < totalHashes) {
        const withHash = Array.from(t);
        withHash[index] = "#";
        if (isMatching(withHash, runLengths) != MatchResult.IMPOSSIBLE) {
          queue.push([withHash, hashCount + 1]);
        }
      }
    }
  }
}

enum MatchResult {
  YES,
  MAYBE,
  IMPOSSIBLE,
}

function isMatching(pattern: string[], runLengths: number[]) {
  let index = 0;
  let inHash = false;
  let runLengthIndex = -1;
  let runLengthCount = 0;
  while (index < pattern.length) {
    let ch = pattern[index];

    if (ch === "?") {
      // console.log("Maybe   ", pattern.slice(0, index + 1).join(''))
      return MatchResult.MAYBE;
    }

    if (inHash) {
      if (ch === "#") {
        if (++runLengthCount > runLengths[runLengthIndex]) {
          // console.log("Too many", pattern.slice(0, index + 1).join(''))
          return MatchResult.IMPOSSIBLE;
        }
      } else if (ch === ".") {
        inHash = false;
        if (runLengthCount !== runLengths[runLengthIndex]) {
          // console.log("Too few ", pattern.slice(0, index + 1).join(''))
          return MatchResult.IMPOSSIBLE;
        }
      }
    } else {
      if (ch === ".") {
        // nop
      } else if (ch === "#") {
        inHash = true;
        runLengthCount = 1;
        if (++runLengthIndex >= runLengths.length) {
          // console.log("Too many # groups", pattern.slice(0, index + 1).join(''))
          return MatchResult.IMPOSSIBLE;
        }
      } else {
        throw new Error("Parse error");
      }
    }
    ++index;
  }
  if (inHash) {
    if (runLengthCount !== runLengths[runLengthIndex]) {
      // console.log("Too few #", pattern.slice(0, index + 1).join(''))
      return MatchResult.IMPOSSIBLE;
    }
  }

  const result =
    runLengthIndex === runLengths.length - 1
      ? MatchResult.YES
      : MatchResult.IMPOSSIBLE;
  // console.log(pattern.join(''), result)
  return result;
}

type Combo3State = {
  template: string[];
  runLengths: number[];
  runLengthCount: number;
};
const combo3Memo = new Map<Combo3State, number>();

function combo3(state: Combo3State, prefix = ""): number {
  state.template = simplify(state.template);
  // console.log(state.template.join(''), state.runLengths.join())

  const memoizedResult = combo3Memo.get(state);
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
          tryCache(state, 0);
          return 0;
        } else {
          assert.fail();
        }
      } else if (runLengthCount === runLengths[runLengthIndex]) {
        if (ch === "?") {
          template[position] = ".";
          runLengthCount = 0;
          ++runLengthIndex;
        } else if (ch === "#") {
          tryCache(state, 0);
          return 0;
        } else if (ch === ".") {
          runLengthCount = 0;
          ++runLengthIndex;
        } else {
          assert.fail();
        }
      } else {
        tryCache(state, 0);
        return 0;
        // assert.fail(`${runLengthCount} @ ${runLengthIndex}}`)
      }
    } else if (runLengthCount === 0) {
      if (ch === "?") {
        if (runLengthIndex >= runLengths.length) {
          template[position] = ".";
        } else {
          // console.group(' =>')

          const result =
            combo3(
              {
                template: template.slice(position + 1),
                runLengthCount: 1,
                runLengths: runLengths.slice(runLengthIndex),
              },
              prefix + template.slice(0, position).join("") + "#",
            ) +
            combo3(
              {
                template: template.slice(position + 1),
                runLengthCount: 0,
                runLengths: runLengths.slice(runLengthIndex),
              },
              prefix + template.slice(0, position).join("") + ".",
            );

          if (state.template.length < 10) {
            // state.template = simplify(state.template)
            tryCache(state, result);
          }
          // console.groupEnd()

          return result;
        }
      } else if (ch === "#") {
        runLengthCount = 1;
      } else if (ch === ".") {
        // all good
      } else {
        assert.fail();
      }
    } else {
      assert.fail();
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
    tryCache(state, 1);
    // console.log(prefix + template.join(''))
    return 1;
  } else {
    tryCache(state, 0);
    return 0;
  }
}

function simplify(template: string[]) {
  let s = template.join("");
  // s = s.replace(/^\.+/, '')
  s = s.replace(/\.+$/, "");
  s = s.replace(/\.+/g, ".");

  return Array.from(s);
}

let cacheFailed = false;

function tryCache(state: Combo3State, value: number) {
  if (cacheFailed) return;

  try {
    combo3Memo.set(state, value);
  } catch {
    cacheFailed = true;
  }
}
