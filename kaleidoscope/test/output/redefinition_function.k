Optimized LLVM assembly:
; ModuleID = 'kaleidoscope'
source_filename = "<string>"

%IntList = type { i32, %IntList* }

@VoidIntList = local_unnamed_addr constant %IntList* null

declare i32 @printi(i32) local_unnamed_addr

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
define i32 @f(i32 %a_0) local_unnamed_addr #1 {
  %1 = add i32 %a_0, 1
  ret i32 %1
}

; Function Attrs: norecurse nounwind readnone willreturn
define i32 @f.1(i32 %a_0) local_unnamed_addr #1 {
  %1 = add i32 %a_0, 1000
  ret i32 %1
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i32 @f.1(i32 50)
  %2 = tail call i32 @printi(i32 %1)
  ret i32 %2
}

attributes #0 = { norecurse nounwind readonly willreturn }
attributes #1 = { norecurse nounwind readnone willreturn }


1050
