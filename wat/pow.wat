(module
  (func (param $x i32) (param $y i32) (result i32)
    (local $i i32)
    (local $res i32)

    (local.set $res (i32.const 1))
    (loop $loop
      (local.set $res (i32.mul (local.get $res) (local.get $x)))
      (local.set $i (i32.sub (local.get $i) (i32.const 1)))
      (br_if $loop (i32.ne (local.get $i) (local.get $y)))
    )
    (local.get $res)
  )
  (export "pow" (func 0))
)
