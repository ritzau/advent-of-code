import fs from 'fs';
import { readAocInputFile } from './aoclib';

<<<<<<< HEAD
export async function main() {
    const sample1 = fs.readFileSync('./AoC23E01-sample-1.txt').toString();
    const sample2 = fs.readFileSync('./AoC23E01-sample-2.txt').toString();
    const input = await readAocInputFile(1);

    console.log("\nAoC23E01");
    console.log("========");
    console.log("Sample 1:", part1(sample1));
    console.log("Part 1:  ", part1(input));
    console.log("Sample 2:", part2(sample2));
    console.log("Part 2:  ", part2(input));
}

function part1(input: string): any {
    return input.split('\n')
        .map((line) => line.replace(RegExp(/\D+/g), ''))
        .filter((line) => line.length > 0)
        .map((digits) => Array.from(digits))
        .map((line) => line[0] + line[line.length - 1])
        .map((line) => parseInt(line))
        .reduce((a, b) => a + b);
}

function part2(input: string): any {
    return input.split('\n')
        .map((line) => [firstDigit(line), lastDigit(line)])
        .filter((ds) => ds[0] !== -1)
        .map((ds) => 10 * ds[0] + ds[1])
        .reduce((a, b) => a + b);
}

const digitTuples: Array<[string, Number]> = [
    ["0", 0],
    ["1", 1],
    ["2", 2],
    ["3", 3],
    ["4", 4],
    ["5", 5],
    ["6", 6],
    ["7", 7],
    ["8", 8],
    ["9", 9],
    ["one", 1],
    ["two", 2],
    ["three", 3],
    ["four", 4],
    ["five", 5],
    ["six", 6],
    ["seven", 7],
    ["eight", 8],
    ["nine", 9],
];

function firstDigit(line: string): number {
    const matches = digitTuples
        .map(([match, value]) => [line.indexOf(match), match, value])
        .filter(([index, ,]) => index != -1);

    if (matches.length === 0) {
        return -1;
    }

    return matches.reduce((a, b) => a[0] < b[0] ? a : b)[2] as number;
}

function lastDigit(line: string): number {
    const matches = digitTuples
        .map(([match, value]) => [line.lastIndexOf(match), match, value])
        .filter(([index, ,]) => index != -1);

    if (matches.length === 0) {
        return -1;
    }

    return matches.reduce((a, b) => a[0] > b[0] ? a : b)[2] as number;
}
