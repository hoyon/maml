(assert_return (invoke "fibonacci" (i32.const 1)) (i32.const 1))
(assert_return (invoke "fibonacci" (i32.const 7)) (i32.const 21))