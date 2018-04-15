(assert_return (invoke "euler1" (i32.const 10)) (i32.const 23))

;; Can't really test euler5 as reference wasm interpreter can't handle that much recursion

(assert_return (invoke "euler6" (i32.const 100)) (i32.const 25164150))