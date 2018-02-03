var fs = require('fs');

function print(n) {
    console.log('Dec: ' + n + '  \tHex: 0x' + n.toString(16));
}

const importObject = {
    host: {
        print: print
    }
};

async function test() {
    const file = fs.readFileSync('memory.wasm');

    const bytes = Int8Array.prototype.slice.call(file, 0);

    const module = await WebAssembly.instantiate(bytes, importObject);
    const result = module.instance.exports.main();
    console.log('Return value: ' + result);

    console.log('Dumped memory to ./mem_dump');

    fs.writeFileSync('mem_dump', new Buffer(module.instance.exports.mem.buffer));
}

test();
