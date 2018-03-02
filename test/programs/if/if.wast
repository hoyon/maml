(assert_return (invoke "eq" (i32.const 1) (i32.const 1)) (i32.const 1))
(assert_return (invoke "eq" (i32.const 1) (i32.const 2)) (i32.const 0))
