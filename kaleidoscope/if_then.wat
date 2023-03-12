(module
  (type (;0;) (func (result f64)))
  (type (;1;) (func (param f64) (result f64)))
  (func (;0;) (type 1) (param f64) (result f64)
    block  ;; label = @1
      f64.const 0x1p+0 (;=1;)
      f64.sub
      call 0
      f64.const 0x1p+1 (;=2;)
      f64.sub
      call 0
      f64.add
    end
    block  ;; label = @1
      f64.const 0x1.8p+1 (;=3;)
      f64.lt
      if  ;; label = @2
        block  ;; label = @3
          br 2 (;@1;)
        end
      else
        block  ;; label = @3
          br 0 (;@3;)
        end
      end
    end
    block  ;; label = @1
      f64.const 0x1p+0 (;=1;)
    end)
  (func (;1;) (type 0) (result f64)
    block  ;; label = @1
      f64.const 0x1.4p+5 (;=40;)
      call 0
    end)
  (export "main" (func 0))
  (export "fib" (func 1)))
