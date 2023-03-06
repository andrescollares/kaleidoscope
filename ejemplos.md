`ready> 500;`

```
; ModuleID = 'my cool jit'
source_filename = "<string>"

define double @main() {
entry:
  ret double 5.000000e+02
}
```


`ready> def f() 1 + 1;`

```
; ModuleID = 'my cool jit'
source_filename = "<string>"

define double @f() {
entry:
  %0 = fadd double 1.000000e+00, 1.000000e+00
  ret double %0
}
```
