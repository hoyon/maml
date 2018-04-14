(assert_return (invoke "collatz" (i32.const 10)) (i32.const 5))
(assert_return (invoke "collatz" (i32.const 5)) (i32.const 16))

(assert_return (invoke "collatz_length" (i32.const 9)) (i32.const 19))
(assert_return (invoke "collatz_length" (i32.const 871)) (i32.const 178))