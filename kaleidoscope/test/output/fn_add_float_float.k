Optimized LLVM assembly:
; ModuleID = 'kaleidoscope'
source_filename = "<string>"

%IntList = type { i32, %IntList* }

@VoidIntList = local_unnamed_addr constant %IntList* null

declare double @printd(double) local_unnamed_addr

; Function Attrs: norecurse nounwind readonly willreturn
define %IntList* @tail(%IntList* nocapture readonly %list) local_unnamed_addr #0 {
entry:
  %0 = getelementptr inbounds %IntList, %IntList* %list, i64 0, i32 1
  %1 = load %IntList*, %IntList** %0, align 8
  ret %IntList* %1
}

; Function Attrs: norecurse nounwind readonly willreturn
define i32 @head(%IntList* nocapture readonly %list) local_unnamed_addr #0 {
entry:
  %0 = getelementptr inbounds %IntList, %IntList* %list, i64 0, i32 0
  %1 = load i32, i32* %0, align 4
  ret i32 %1
}

; Function Attrs: norecurse nounwind readnone willreturn
define double @add(double %a_0, double %b_0) local_unnamed_addr #1 {
  %1 = fadd double %a_0, %b_0
  ret double %1
}

define double @main() local_unnamed_addr {
  %1 = tail call double @add(double 1.000000e+00, double 2.000000e+00)
  %2 = tail call double @printd(double %1)
  ret double %2
}

attributes #0 = { norecurse nounwind readonly willreturn }
attributes #1 = { norecurse nounwind readnone willreturn }


3.000000
