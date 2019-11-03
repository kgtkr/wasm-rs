(module
  (func (param i32 i32) (result i32)
    get_local 1
    i32.eqz
    if (result i32)
      get_local 0
    else
      get_local 1
      get_local 0
      get_local 1
      i32.rem_s
      call 0
    end
  )
  (export "gcd" (func 0))
)
