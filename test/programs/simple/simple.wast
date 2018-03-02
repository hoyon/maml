;; Test id function
(assert_return (invoke "id" (i32.const 4)) (i32.const 4))
(assert_return (invoke "id" (i32.const 22222222)) (i32.const 22222222))

;; Test add function
;; add(4, 5) = 9
(assert_return (invoke "add" (i32.const 4) (i32.const 5)) (i32.const 9))

;; vim: ft=lisp
