/** AoC 2023 Day 4: Scratchcards */

type Card = {
  cardNumber: number;
  winningNumbers: number[];
  myNumbers: number[];
};

function parseLine(line: string): Card {
  const matches = line.match(/^Card\s+(\d*):\s*(.*?)\s*\|\s*(.*?)\s*$/);
  if (matches === null) throw new Error(`Parse error: ${line}`);
  const [, cardNumber, winningNumbers, myNumbers] = matches;
  return {
    cardNumber: parseInt(cardNumber),
    winningNumbers: winningNumbers.split(/\s+/).map((x) => parseInt(x)),
    myNumbers: myNumbers.split(/\s+/).map((x) => parseInt(x)),
  };
}

function matchingNumbers(card: Card): number {
  const mine = new Set(card.myNumbers);
  return card.winningNumbers.filter((x) => mine.has(x)).length;
}

function pointsScored(card: Card): number {
  const matchCount = matchingNumbers(card);
  return matchCount === 0 ? 0 : Math.pow(2, matchCount - 1);
}

export function solvePart1(input: string): number {
  const lines = input.split("\n");
  let total = 0;
  for (const line of lines) {
    if (line.trim().length === 0) continue;
    const card = parseLine(line);
    total += pointsScored(card);
  }
  return total;
}

export function solvePart2(input: string): number {
  const lines = input.split("\n").filter((line) => line.trim().length > 0);
  const copies: number[] = [];

  for (const line of lines) {
    const card = parseLine(line);
    const multiplier = getAndEnsureInitialized(card.cardNumber);
    const matchCount = matchingNumbers(card);

    for (let i = 1; i <= matchCount; ++i) {
      const cardNumber = card.cardNumber + i;
      const current = copies[cardNumber] ?? 1;
      copies[cardNumber] = current + multiplier;
    }
  }

  return copies.reduce((a, b) => a + b);

  function getAndEnsureInitialized(index: number) {
    if (copies[index] === undefined) {
      copies[index] = 1;
    }
    return copies[index];
  }
}
