(module
  (func (param $x i32) (result i32)
    (block $a
        (block $b
            (block $c
                get_local $x
                br_table $a $b $c
            )
            i32.const 30
            return
        )
        i32.const 20
        return
    )
    i32.const 10
  )
  (export "br_table" (func 0))
)
