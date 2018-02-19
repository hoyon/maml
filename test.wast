(assert_return (invoke "id" (i32.const 4)) (i32.const 4))
(invoke "f1" (i32.const 4) (i32.const 5))
(invoke "f2" (i32.const 4))
(invoke "addY" (i32.const 2))

;; vim: ft=lisp
