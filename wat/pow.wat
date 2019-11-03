(module
  (func (param $x i32) (param $y i32) (result i32)
    (local $i i32)
    (local $res i32)

    (set_local $res (i32.const 1))
    (loop $loop
      (set_local $res (i32.mul (get_local $res) (get_local $x)))
      (set_local $i (i32.add (get_local $i) (i32.const 1)))
      (br_if $loop (i32.ne (get_local $i) (get_local $y)))
    )
    (get_local $res)
  )
  (export "pow" (func 0))
)
