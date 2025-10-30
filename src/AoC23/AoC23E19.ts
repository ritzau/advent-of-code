import assert from "node:assert";
import { readFileSync } from "node:fs";

import { logResult } from "./aoclib";

export async function main() {
  logResult("Part 1 - sample", part1("AoC23E19-sample.txt"), 19114);
  logResult("Part 1 - input", part1("AoC23E19-input.txt"), 383682);
  logResult("Part 2 - sample", part2("AoC23E19-sample.txt"), 167409079868000);
  logResult("Part 2 - input", part2("AoC23E19-input.txt"), 117954800808317);
}

type Condition = {
  opA: "x" | "m" | "a" | "s";
  op: "<" | ">";
  opB: number;
  destination: string;
};

type Workflow = {
  name: string;
  conditions: Condition[];
  defaultDestination: string;
};

type Part = {
  x: number;
  m: number;
  a: number;
  s: number;
};

type Range = {
  min: number;
  max: number;
};

type PartCombination = {
  x: Range;
  m: Range;
  a: Range;
  s: Range;
};

function part1(path: string) {
  const [workflows, parts] = parseWorkflowParts(path);

  return parts
    .filter((p) => processPart(p, "in", workflows))
    .map(({ x, m, a, s }) => x + m + a + s)
    .reduce((a, b) => a + b);
}

function part2(path: string) {
  const [workflows] = parseWorkflowParts(path);
  return countAcceptedCombinations(workflows);
}

function parseWorkflowParts(
  path: string,
): [workflows: Map<string, Workflow>, parts: Part[]] {
  const [workflowLines, partLines] = readFileSync(path)
    .toString()
    .split("\n\n")
    .map((ls) => ls.split("\n").filter((l) => l.length > 0));

  return [parseWorkflows(workflowLines), parseParts(partLines)];

  function parseWorkflows(lines: string[]) {
    return new Map(
      lines.map((l) => {
        const [, name, cs, defaultDestination] =
          l.match(/(\w+){(\w+[<>]\d+:\w+(?:,\w+[<>]\d+:\w+)*),(\w+)}/) ??
          assert.fail(l);

        const conditions = cs.split(",").map((c) => {
          const [, opA, op, opB, destination] =
            c.match(/(\w+)([<>])(\d+):(\w+)/) ?? assert.fail(c);
          return { opA, op, opB: parseInt(opB), destination };
        });

        return [name, <Workflow>{ name, conditions, defaultDestination }];
      }),
    );
  }

  function parseParts(lines: string[]) {
    return lines.map((l) => {
      const match =
        l.match(/{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}/) ?? assert.fail(l);
      const [x, m, a, s] = match.slice(1).map((x) => parseInt(x));
      return <Part>{ x, m, a, s };
    });
  }
}

function processPart(
  part: Part,
  flowName: string,
  flows: Map<string, Workflow>,
) {
  const flow = flows.get(flowName) ?? assert.fail(flowName);

  const destination = processFlow(part, flow);
  switch (destination) {
    case "A":
      return true;
    case "R":
      return false;
    default:
      return processPart(part, destination, flows);
  }
}

function processFlow(part: Part, flow: Workflow) {
  return (
    flow.conditions.find(evalCondition)?.destination ?? flow.defaultDestination
  );

  function evalCondition(c: Condition) {
    const a = getOperand(c.opA);
    switch (c.op) {
      case "<":
        return a < c.opB;
      case ">":
        return a > c.opB;
      default:
        assert.fail(c.op);
    }
  }

  function getOperand(name: string) {
    switch (name) {
      case "x":
        return part.x;
      case "m":
        return part.m;
      case "a":
        return part.a;
      case "s":
        return part.s;
      default:
        assert.fail(name);
    }
  }
}

function countAcceptedCombinations(flows: Map<string, Workflow>) {
  const fullCombo = <PartCombination>{
    x: { min: 1, max: 4000 },
    m: { min: 1, max: 4000 },
    a: { min: 1, max: 4000 },
    s: { min: 1, max: 4000 },
  };

  return process(fullCombo, "in");

  function process(combo: PartCombination, name: string) {
    switch (name) {
      case "R":
        return 0;
      case "A":
        return comboCount(combo);
    }

    const flow = flows.get(name) ?? assert.fail(name);

    let sum = 0;
    let currentCombo = combo;
    for (const condition of flow.conditions) {
      const [thenCombo, elseCombo] = restrictCombo(currentCombo, condition);
      sum += process(thenCombo, condition.destination);
      currentCombo = elseCombo;
    }
    sum += process(currentCombo, flow.defaultDestination);

    return sum;
  }

  function comboCount(combo: PartCombination) {
    return (
      rangeCount(combo.x) *
      rangeCount(combo.m) *
      rangeCount(combo.a) *
      rangeCount(combo.s)
    );
  }

  function rangeCount(r: Range) {
    return r.max - r.min + 1;
  }

  function restrictCombo(
    pc: PartCombination,
    condition: Condition,
  ): [thenCombo: PartCombination, elseCombo: PartCombination] {
    switch (condition.opA) {
      case "x":
        return [
          { ...pc, x: rangeThen(pc.x, condition.op, condition.opB) },
          { ...pc, x: rangeElse(pc.x, condition.op, condition.opB) },
        ];
      case "m":
        return [
          { ...pc, m: rangeThen(pc.m, condition.op, condition.opB) },
          { ...pc, m: rangeElse(pc.m, condition.op, condition.opB) },
        ];
      case "a":
        return [
          { ...pc, a: rangeThen(pc.a, condition.op, condition.opB) },
          { ...pc, a: rangeElse(pc.a, condition.op, condition.opB) },
        ];
      case "s":
        return [
          { ...pc, s: rangeThen(pc.s, condition.op, condition.opB) },
          { ...pc, s: rangeElse(pc.s, condition.op, condition.opB) },
        ];
      default:
        assert.fail(condition.opA);
    }

    function rangeThen({ min, max }: Range, op: string, opB: number) {
      switch (op) {
        case "<":
          return { min, max: opB - 1 };
        case ">":
          return { min: opB + 1, max };
        default:
          assert.fail(op);
      }
    }

    function rangeElse({ min, max }: Range, op: string, opB: number) {
      switch (op) {
        case "<":
          return { min: opB, max };
        case ">":
          return { min, max: opB };
        default:
          assert.fail(op);
      }
    }
  }
}
