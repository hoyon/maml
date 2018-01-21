(module
    (memory 1)
    (global (export "x") i32 (i32.const 4))
    (func (export "id") (param i32) (result i32)
     get_local 0
     )
    (func (export "add") (param i32 i32) (result i32)
     get_local 0
     get_local 1
     i32.add
     )
 )
