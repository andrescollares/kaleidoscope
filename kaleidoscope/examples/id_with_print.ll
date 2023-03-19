; ModuleID = 'my cool jit'
source_filename = "<string>"

define double @id(double %x) {
entry:
  %0 = alloca double, align 8
  store double %x, double* %0, align 8
  %1 = load double, double* %0, align 8
  ret double %1
}

; https://stackoverflow.com/questions/69636349/how-to-print-floats-from-llvm-ir
@.fstr = private unnamed_addr constant [8 x i8] c"%f\0A\00\0A\00\0A\00"

declare i32 @printf(i8*, ...)

define double @main() {
entry:
  %0 = call double @id(double 0x430FEE0B99F3FB98)
  %1 = getelementptr [8 x i8],[8 x i8]* @.fstr, i64 0, i64 0
  %2 = call i32 (i8*, ...) @printf(i8* %1, double %0)
  ret double %0
}