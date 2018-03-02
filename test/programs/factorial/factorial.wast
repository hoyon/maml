;; Test factorial function
(assert_return (invoke "factorial" (i32.const 5)) (i32.const 120))
(assert_return (invoke "factorial" (i32.const 6)) (i32.const 720))
(assert_return (invoke "factorial" (i32.const 7)) (i32.const 5040))
(assert_return (invoke "factorial" (i32.const 8)) (i32.const 40320))

;; vim: ft=lisp
