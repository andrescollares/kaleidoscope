; ModuleID = 'basic'
source_filename = "<string>"

define i32 @add(i32 %a, i32 %b) {
entry:
  %result = add i32 %a, %b
  ret i32 %result
}

define i32 @sub(i32 %a, i32 %b) {
entry:
  %result = sub i32 %a, %b
  ret i32 %result
}

define double @id(double* %a) {
entry:
  %tmp_input_w0 = getelementptr inbounds double, double* %a, i64 0
  %0 = load double, double* %tmp_input_w0, align 8
  ret double %0
}

define double @main() #0 {
entry:
  %a = alloca double, align 8
  ;1123432234123123.0 = 152 truncated to 8 bits. echo $? will return 152
  store double 1123432234123123.0, double* %a, align 8
  %0 = call double @id(double* %a)
  ret double %0
}