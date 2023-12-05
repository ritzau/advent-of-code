const {
    Worker: WorkerX, isMainThread, parentPort, workerData, threadId,
} = require('node:worker_threads')

if (isMainThread) {
    module.exports = function (path, start, length) {
        return new Promise((resolve, reject) => {
            const worker = new WorkerX(__filename, {
                workerData: { path, start, length },
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
    const { minLocationFromFile } = require("./AoC23E05-lib")
    const { path, start, length } = workerData;

    minLocationFromFile(path, start, length)
        .then((result) => { parentPort?.postMessage(result) })
}
