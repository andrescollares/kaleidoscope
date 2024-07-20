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
define i32 @sum(i32 %a_0) local_unnamed_addr #1 {
  ret i32 37
}

; Function Attrs: nounwind readnone
define i32 @sum.1(i32 %a_0) local_unnamed_addr #2 {
  %1 = icmp eq i32 %a_0, 0
  br i1 %1, label %if.exit_0, label %if.else_0.preheader

if.else_0.preheader:                              ; preds = %0
  %2 = add i32 %a_0, -1
  %3 = mul i32 %2, %2
  %4 = zext i32 %2 to i33
  %5 = add i32 %a_0, -2
  %6 = zext i32 %5 to i33
  %7 = mul i33 %4, %6
  %8 = lshr i33 %7, 1
  %9 = trunc i33 %8 to i32
  %10 = add i32 %3, %a_0
  %11 = sub i32 %10, %9
  br label %if.exit_0

if.exit_0:                                        ; preds = %if.else_0.preheader, %0
  %accumulator.tr.lcssa = phi i32 [ 0, %0 ], [ %11, %if.else_0.preheader ]
  ret i32 %accumulator.tr.lcssa
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i32 @sum.1(i32 10)
  %2 = tail call i32 @printi(i32 %1)
  ret i32 %2
}

attributes #0 = { norecurse nounwind readonly willreturn }
attributes #1 = { norecurse nounwind readnone willreturn }
attributes #2 = { nounwind readnone }


55
