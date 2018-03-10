;; Test id function
(assert_return (invoke "id" (i32.const 4)) (i32.const 4))
(assert_return (invoke "id" (i32.const 22222222)) (i32.const 22222222))

;; Test add function
;; add(4, 5) = 9
(assert_return (invoke "add" (i32.const 4) (i32.const 5)) (i32.const 9))

;; Test quintuple function
;; quintuple(5) = 25
(assert_return (invoke "quintuple" (i32.const 5)) (i32.const 25))

;; vim: ft=lisp
