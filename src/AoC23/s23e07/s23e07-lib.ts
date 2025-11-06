/** AoC 2023 Day 7: Camel Cards */

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

function parseLine(line: string): Bid {
  const match = line.match(/^([2-9TJQKA]{5})\s(\d+)$/);
  if (match === null) throw new Error(`Parse error: ${line}`);
  const [, handString, bidString] = match;

  return {
    hand: parseHand(handString),
    bid: parseInt(bidString),
  };
}

function parseLineWithJokers(line: string): Bid {
  const match = line.match(/^([2-9TJQKA]{5})\s(\d+)$/);
  if (match === null) throw new Error(`Parse error: ${line}`);
  const [, handString, bidString] = match;

  return {
    hand: parseHandWithJokers(handString),
    bid: parseInt(bidString),
  };
}

function parseHand(handString: string): Hand {
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

function parseHandWithJokers(handString: string): Hand {
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

function handType(frequencies: string): HandType {
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
  if (type === undefined) {
    throw new Error(`Unknown frequency pattern: ${frequencies}`);
  }

  return type;
}

function rankOfCard(card: string): number {
  const rank = "AKQJT98765432".indexOf(card);
  if (rank === -1) {
    throw new Error(`Unknown card: ${card}`);
  }
  return rank;
}

function rankOfCardWithJokers(card: string): number {
  const rank = "AKQT98765432J".indexOf(card);
  if (rank === -1) {
    throw new Error(`Unknown card: ${card}`);
  }
  return rank;
}

function compareHands(a: Hand, b: Hand): number {
  if (a.type < b.type) return -1;
  if (a.type > b.type) return 1;

  for (let i = 0; i < a.cardRanks.length; ++i) {
    if (a.cardRanks[i] < b.cardRanks[i]) return -1;
    if (a.cardRanks[i] > b.cardRanks[i]) return 1;
  }

  return 0;
}

export function solvePart1(input: string): number {
  const lines = input.split("\n");
  const bids: Bid[] = [];

  for (const line of lines) {
    if (line.trim().length === 0) continue;
    bids.push(parseLine(line));
  }

  return bids
    .sort((a, b) => -compareHands(a.hand, b.hand))
    .map((b, i) => b.bid * (i + 1))
    .reduce((a, b) => a + b);
}

export function solvePart2(input: string): number {
  const lines = input.split("\n");
  const bids: Bid[] = [];

  for (const line of lines) {
    if (line.trim().length === 0) continue;
    bids.push(parseLineWithJokers(line));
  }

  return bids
    .sort((a, b) => -compareHands(a.hand, b.hand))
    .map((b, i) => b.bid * (i + 1))
    .reduce((a, b) => a + b);
}
