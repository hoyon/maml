var fs = require('fs');

function print(n) {
    console.log('Dec: ' + n + '  \tHex: 0x' + n.toString(16));
}

async function test() {
    const file = fs.readFileSync('memory.wasm');

    const bytes = Int8Array.prototype.slice.call(file, 0);

    const module = await WebAssembly.instantiate(bytes);
    const exports = module.instance.exports;
    
    test_main(exports);
    test_global(exports);

    // Dump memory to file
    fs.writeFileSync('mem_dump', new Buffer(module.instance.exports.mem.buffer));
    console.log('Dumped memory to ./mem_dump');
}

function test_main(exports) {
    const result = exports.main();
    console.log('Return value: ' + result);
}

function test_global(exports) {
    const value = 10;
    const index = exports.allocate_global_i32(value);
    if (value == exports.get_global_i32) {
        console.log("Failed")
    } else {
        console.log("Pass")
    }
}

test();
