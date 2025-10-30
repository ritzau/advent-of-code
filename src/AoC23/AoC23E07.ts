import * as readline from "node:readline";

import { createReadStream } from "fs";
import assert from "node:assert";
import { logResult } from "./aoclib";

export async function main() {
  logResult("Part 1 - sample", await part1("AoC23E07-sample.txt"), 6440);
  logResult("Part 1 - input", await part1("AoC23E07-input.txt"), 253933213);
  logResult("Part 2 - sample", await part2("AoC23E07-sample.txt"), 5905);
  logResult("Part 2 - input", await part2("AoC23E07-input.txt"), 253473930);
}

enum HandType {
  FIVE_OF_A_KIND,
  FOUR_OF_A_KIND,
  FULL_HOUSE,
  THREE_OF_A_KIND,
  TWO_PAIRS,
  ONE_PAIR,
  HIGH_CARD,
}

type Hand = {
  cardRanks: number[];
  type: HandType;
};

type Bid = {
  hand: Hand;
  bid: number;
};

async function part1(path: string) {
  const reader = createReadStream(path);
  const input = readline.createInterface(reader);

  const bids: Bid[] = [];
  for await (const line of input) {
    bids.push(parseLine(line));
  }

  return bids
    .sort((a, b) => -compareHands(a.hand, b.hand))
    .map((b, i) => b.bid * (i + 1))
    .reduce((a, b) => a + b);
}

async function part2(path: string) {
  const reader = createReadStream(path);
  const input = readline.createInterface(reader);

  const bids: Bid[] = [];
  for await (const line of input) {
    bids.push(parseLineWithJokers(line));
  }

  return bids
    .sort((a, b) => -compareHands(a.hand, b.hand))
    .map((b, i) => b.bid * (i + 1))
    .reduce((a, b) => a + b);
}

function parseLine(line: string) {
  const match = line.match(/^([2-9TJQKA]{5})\s(\d+)$/);
  if (match === null) throw new Error(`Parse error: ${line}`);
  assert.strictEqual(match.length, 3);
  const [, handString, bidString] = match;

  return {
    hand: parseHand(handString),
    bid: parseInt(bidString),
  };
}

function parseLineWithJokers(line: string) {
  const match = line.match(/^([2-9TJQKA]{5})\s(\d+)$/);
  if (match === null) throw new Error(`Parse error: ${line}`);
  assert.strictEqual(match.length, 3);
  const [, handString, bidString] = match;

  return {
    hand: parseHandWithJokers(handString),
    bid: parseInt(bidString),
  };
}

function parseHand(handString: string) {
  assert.strictEqual(handString.length, 5);

  const frequency = Array.from(handString).reduce(
    (m, c) => m.set(c, (m.get(c) ?? 0) + 1),
    new Map<string, number>(),
  );
  const frequencyString = Array.from(frequency.values())
    .sort()
    .reverse()
    .join("");

  return {
    cardRanks: Array.from(handString).map((c) => rankOfCard(c)),
    type: handType(frequencyString),
  };
}

function parseHandWithJokers(handString: string) {
  assert.strictEqual(handString.length, 5);

  const frequency = Array.from(handString).reduce(
    (m, c) => m.set(c, (m.get(c) ?? 0) + 1),
    new Map<string, number>(),
  );

  const jokerCount = frequency.get("J") ?? 0;
  frequency.delete("J");

  const frequencyString = Array.from(frequency.values())
    .sort()
    .reverse()
    .map((v, i) => (i === 0 ? v + jokerCount : v))
    .join("");

  return {
    cardRanks: Array.from(handString).map((c) => rankOfCardWithJokers(c)),
    type: handType(frequencyString.length === 0 ? "5" : frequencyString),
  };
}

function handType(frequencies: string) {
  const typeMap: { [key: string]: HandType } = {
    "5": HandType.FIVE_OF_A_KIND,
    "41": HandType.FOUR_OF_A_KIND,
    "32": HandType.FULL_HOUSE,
    "311": HandType.THREE_OF_A_KIND,
    "221": HandType.TWO_PAIRS,
    "2111": HandType.ONE_PAIR,
    "11111": HandType.HIGH_CARD,
  };

  const type = typeMap[frequencies];
  assert.notStrictEqual(type, undefined);

  return type;
}

function rankOfCard(card: string) {
  assert.strictEqual(1, card.length);
  const rank = "AKQJT98765432".indexOf(card);
  assert.notStrictEqual(rank, -1);

  return rank;
}

function rankOfCardWithJokers(card: string) {
  assert.strictEqual(1, card.length);
  const rank = "AKQT98765432J".indexOf(card);
  assert.notStrictEqual(rank, -1);

  return rank;
}

function compareHands(a: Hand, b: Hand) {
  if (a.type < b.type) return -1;
  if (a.type > b.type) return 1;

  assert.strictEqual(a.cardRanks.length, b.cardRanks.length);

  for (let i = 0; i < a.cardRanks.length; ++i) {
    if (a.cardRanks[i] < b.cardRanks[i]) return -1;
    if (a.cardRanks[i] > b.cardRanks[i]) return 1;
  }

  return 0;
}
