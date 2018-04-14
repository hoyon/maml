(assert_return (invoke "even" (i32.const 1)) (i32.const 0))
(assert_return (invoke "even" (i32.const 0)) (i32.const 1))

(assert_return (invoke "odd" (i32.const 0)) (i32.const 0))
(assert_return (invoke "odd" (i32.const 1)) (i32.const 1))

(assert_return (invoke "f" (i32.const 3) (i32.const 2)) (i32.const 2))