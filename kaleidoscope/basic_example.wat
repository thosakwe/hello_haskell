(module
  (type (;0;) (func (result f64)))
  (type (;1;) (func (param f64) (result f64)))
  (func (;0;) (type 0) (result f64))
  (func (;1;) (type 1) (param f64) (result f64)
    block  ;; label = @1
      f64.const 0x1p+1 (;=2;)
      f64.mul
    end)
  (export "main" (func 0))
  (export "times_two" (func 1)))
