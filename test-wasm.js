#!/bin/env node

// A small script which loads a WebAssembly file and exposes all exported
// functions into the repl. All maml functions can be called from the repl.

const fs = require('fs');
const repl = require('repl');

const args = process.argv.slice(2);
if (args.length != 1) {
    console.error("Usage: node test-wasm.js [wasm file]");
    process.exit(1);
}
const context = repl.start(args[0] + '> ').context;

async function loadFile(filename) {
    try {
        const file = fs.readFileSync(filename);
        const bytes = Int8Array.prototype.slice.call(file, 0);

        const module = await WebAssembly.instantiate(bytes);
        const exports = module.instance.exports;
        Object.keys(exports).forEach(function(key) {
            context[key] = exports[key];
        });
    } catch(e) {
        console.error("Failed to load wasm file: ", e);
        process.exit(1);
    }
}
loadFile(args[0]);
