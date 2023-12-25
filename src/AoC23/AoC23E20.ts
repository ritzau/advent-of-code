import assert from "node:assert";
import { readFileSync } from "node:fs";
import { logResult } from "./aoclib";

export async function main() {
    logResult("Part 1 - sample 1", part1('AoC23E20-sample-1.txt'), 32000000);
    logResult("Part 1 - sample 2", part1('AoC23E20-sample-2.txt'), 11687500);
    logResult("Part 1 - input", part1('AoC23E20-input.txt'), 899848294);
    logResult("Part 2 - input", part2('AoC23E20-input.txt'), 11687500);
}

type UntypedModule = {
    kind: '';
    name: string;
    destinations: string[];
    count: number;
};

type FlipFlopModule = {
    kind: '%';
    name: string;
    destinations: string[];
    state: 'on' | 'off';
};

type ConjunctionModule = {
    kind: '&';
    name: string;
    inputs: Map<string, 'low' | 'high'>;
    destinations: string[];
};

type Module = UntypedModule | FlipFlopModule | ConjunctionModule;

type Modules = Map<string, Module>;

type Signal = {
    kind: 'low' | 'high';
    source: string;
    destination: string;
};

function part1(path: string) {
    const modules = parse(readFile(path));
    preprocessModules(modules);

    let countLow = 0, countHigh = 0;
    for (let i = 0; i < 1000; ++i) {
        const [low, high] = processButtonPress(modules);
        countLow += low;
        countHigh += high;
    }

    return countLow * countHigh;
}

function part2(path: string) {
    const modules = parse(readFile(path));
    // console.log(modules);
    preprocessModules(modules);

    const rx = modules.get('rx');
    if (rx === undefined) assert.fail();
    if (rx.kind !== '') assert.fail();

    for (let i = 0; i < Number.MAX_SAFE_INTEGER; ++i) {
        if (i % 1000000 === 0) {
            console.log(i);
        }
        // console.log();
        processButtonPress(modules);

        // console.log(rx.count);
        if (rx.count === 1) {
            return i + 1;
        }
        rx.count = 0;
    }

    return 0;
}

function readFile(path: string) {
    return readFileSync(path)
        .toString()
        .split('\n')
        .filter(l => l.length > 0);
}

function parse(lines: string[]) {
    return new Map(lines.map(l => {
        const [, kind, name, destinationList] =
            l.match(/^([%&])?([a-z]+) -> (\b.*\b)$/) ?? assert.fail(`"${l}"`);
        const destinations = destinationList.split(',').map(d => d.trim());

        switch (kind) {
            case undefined: return [name, <Module>{ name, kind: '', destinations, count: 0 }];
            case '%': return [name, <Module>{ name, kind, destinations, state: 'off' }];
            case '&': return [name, <Module>{ name, kind, destinations, inputs: new Map() }];
            default: assert.fail(`Wrong kind: "${kind}"`);
        }
    }));
}

function preprocessModules(modules: Modules) {
    for (const module of modules.values()) {
        module.destinations
            .map(n => {
                const d = modules.get(n);
                if (d === undefined) {
                    const m = <Module>{ name: n, kind: '', destinations: [], count: 0 };
                    modules.set(n, m);
                    return m;
                }
                return d;
            })
            .forEach(m => {
                if (m.kind === '&') {
                    m.inputs.set(module.name, 'low');
                }
            });
    }
}

function processButtonPress(modules: Modules) {
    const queue = [<Signal>{ kind: 'low', source: 'button', destination: 'broadcaster' }];
    let signal: Signal | undefined;
    let countLow = 0, countHigh = 0;
    while ((signal = queue.shift()) != undefined) {
        if (signal.kind === 'low') ++countLow;
        else ++countHigh;
        processSignal(signal);
    }

    return [countLow, countHigh];

    function processSignal({ kind, source, destination }: Signal) {
        const destinationModule: Module = modules.get(destination) ?? assert.fail(destination);
        // console.log(source, `-${kind}->`, destination);
        switch (destinationModule.kind) {
            case '':
                destinationModule.destinations.forEach(d =>
                    queue.push(<Signal>{ kind, source: destination, destination: d }));
                if (kind === 'low' && destination === 'rx') {
                    console.log(kind, destination);
                    ++destinationModule.count;
                }
                break;
            case '%':
                if (kind === 'low') {
                    destinationModule.state = destinationModule.state === 'off' ? 'on' : 'off';
                    // console.log("XXX flip state", destination, destinationModule.state);
                    destinationModule.destinations.forEach(d =>
                        queue.push(<Signal>{ kind: destinationModule.state === 'on' ? 'high' : 'low', source: destination, destination: d }));

                }
                break;
            case '&': {
                const inputs = destinationModule.inputs;
                inputs.set(source, kind);
                // console.log("XXX con", destination, inputs);
                const signalKind = [...inputs.values()].every(v => v === 'high') ? 'low' : 'high';
                destinationModule.destinations.forEach(d =>
                    queue.push(<Signal>{ kind: signalKind, source: destination, destination: d }));
                break;
            }
            default:
                assert.fail();
        }

    }
}
