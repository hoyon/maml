all: memory.wasm

test: memory.wasm
	node test.js

memory.wasm: memory.wat
	wat2wasm memory.wat -o memory.wasm

.SILENT:run
