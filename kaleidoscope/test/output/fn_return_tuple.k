Optimized LLVM assembly:
; ModuleID = 'kaleidoscope'
source_filename = "<string>"

%IntList = type { i32, %IntList* }

@VoidIntList = local_unnamed_addr constant %IntList* null

declare i32 @print_tuple({ i32, i32 }, i32, i32) local_unnamed_addr

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
define { double, double } @origin() local_unnamed_addr #1 {
  ret { double, double } zeroinitializer
}

define { double, double } @main() local_unnamed_addr {
  %1 = tail call i32 @print_tuple({ double, double } zeroinitializer, i32 2, i32 2)
  ret i32 %1
}

attributes #0 = { norecurse nounwind readonly willreturn }
attributes #1 = { norecurse nounwind readnone willreturn }


(0.000000, 