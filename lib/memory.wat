;; Memory layout:
;; First free memory block: 4 bytes
;;
;; Global Table:
;;   Header:
;;     Entry count: 4 bytes
;;   Entry:
;;     Pointer to allocated memory: 4 bytes
;;   Size:
;;     Header + (256 * Entry) = 4 + 1024 = 1028
;;
;; Total size of reserved memory: 4 + 1028 = 1032

(module
  (memory (export "mem") 1)

  ;; Initialise memory
  (func $init (export "init")
        ;; First free block of memory
        (i32.store (i32.const 0) (i32.const 1032))
        )

  (func $first_free (result i32)
        (i32.load (i32.const 0))
        )

  ;; Allocate a given amount of bytes
  (func $allocate (export "allocate")
        (param
          i32) ;; number of bytes to allocate
        (result
          i32) ;; start of allocated block
        (local
          i32  ;; first free block
          i32) ;; new first free block

        (i32.load (i32.const 0))
        set_local 1

        ;; Store size of memory block
        (i32.store (get_local 1) (get_local 0))

        get_local 0 ;; amount to allocate
        get_local 1 ;; start of header
        i32.const 4 ;; size of header
        i32.add
        i32.add
        set_local 2 ;; first unallocated block

        ;; Update next free block
        (i32.store (i32.const 0) (get_local 2))

        ;; Return position of memory
        get_local 1 ;; start of header
        i32.const 4 ;; size of header
        i32.add
        )

  (func $allocate_global (export "allocate_global")
        (param
          i32) ;; Size of global
        (result
          i32) ;; Index of global
        (local
          i32  ;; Address of allocated memory
          i32) ;; Index of global

        get_local 0 ;; Size to allocate
        call $allocate
        set_local 1 ;; Store address

        (i32.load (i32.const 4)) ;; Get global entry count
        i32.const 4 ;; 4 bytes each
        i32.mul
        i32.const 8 ;; Address of first entry
        i32.add     ;; Address of new entry

        get_local 1 ;; Address of allocated memory
        i32.store

        (i32.load (i32.const 4)) ;; Get global entry count
        set_local 2 ;; Store index

        (i32.store (i32.const 4) (i32.add (get_local 2) (i32.const 1)))

        get_local 2 ;; Return index of global
        )

  (func $allocate_global_i32 (export "allocate_global_i32")
        (param
         i32) ;; Initial value
        (result
         i32) ;; Index of global
        (local
         i32) ;; Index of global

        (call $allocate_global (i32.const 4)) ;; allocate 4 bytes (sizeof i32)
        tee_local 1 ;; store index
        call $get_global
        get_local 0 ;; initial value
        i32.store 
        
        get_local 1 ;; return index
   )

  (func $get_global (export "get_global")
        (param
          i32) ;; Index of global
        (result
          i32) ;; Address of global
        get_local 0
        i32.const 4 ;; 4 bytes per entry
        i32.mul
        i32.const 8 ;; Address of first entry
        i32.add
        i32.load ;; Get address stored in this entry
        )

  (func (export "main") (result i32)
        call $init

        (call $allocate_global (i32.const 64))
        drop
        (call $allocate_global (i32.const 8))
        drop

        (call $get_global (i32.const 0))
        i32.const -1
        i32.store

        (call $get_global (i32.const 1))
        drop

        ;; call $first_free
        ;; (i32.load (i32.const 0))
        (call $allocate_global_i32 (i32.const 100))
        call $get_global
        i32.load
        )
  )

;; vim: ft=lisp
