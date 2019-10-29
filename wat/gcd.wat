(module
  (func (param i32 i32) (result i32)
    local.get 1
    i32.eqz
    if (result i32)
      local.get 0
    else
      local.get 1
      local.get 0
      local.get 1
      i32.rem_s
      call 0
    end
  )
)
