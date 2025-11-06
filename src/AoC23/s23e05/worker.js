/**
 * UNUSED: Experimental Worker Thread for Day 5 Part 2 Parallelization
 *
 * This worker was designed to parallelize the computation of minimum locations
 * across seed ranges for Part 2, which can be computationally intensive.
 *
 * Current Status: NOT INTEGRATED
 * - The current implementation (s23e05-lib.ts) solves Part 2 sequentially
 * - This worker would need updates to work with the current implementation
 *
 * How to Use (if you want to implement parallel processing):
 *
 * 1. Update the worker to use the current API:
 *    - Import { minLocationForRange, SeedMapMap, parseMaps } from "./lib"
 *    - Pass the input string and seed range (start, length) as workerData
 *    - Compute minLocationForRange(maps, start, length) in the worker
 *
 * 2. Update solvePart2 in s23e05-lib.ts:
 *    - Import this worker module
 *    - Create a worker for each seed range
 *    - Use Promise.all() to wait for all workers to complete
 *    - Return the minimum of all results
 *
 * Example usage pattern:
 *   const workers = seedRanges.map(([start, length]) =>
 *     spawnWorker(input, start, length)
 *   );
 *   const results = await Promise.all(workers);
 *   return Math.min(...results);
 *
 * Note: Worker threads add complexity and overhead. Only beneficial for
 * very large inputs where parallel computation outweighs the overhead.
 */

const {
    Worker: WorkerX, isMainThread, parentPort, workerData, threadId,
} = require('node:worker_threads')

if (isMainThread) {
    module.exports = function (input, start, length) {
        return new Promise((resolve, reject) => {
            const worker = new WorkerX(__filename, {
                workerData: { input, start, length },
            });
            worker.on('message', resolve);
            worker.on('error', reject);
            worker.on('exit', (code) => {
                if (code !== 0)
                    reject(new Error(`Worker stopped with exit code ${code}`));
            });
        });
    };
} else {
    require('ts-node').register()
    const { minLocationForRange, SeedMapMap, parseMaps } = require("./lib")
    const { input, start, length } = workerData;

    const maps = new SeedMapMap(parseMaps(input));
    const result = minLocationForRange(maps, start, length);
    parentPort?.postMessage(result);
}
