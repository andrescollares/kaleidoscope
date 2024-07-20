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

; Function Attrs: nounwind readnone
define i32 @fib(i32 %x_0) local_unnamed_addr #1 {
  %1 = icmp ult i32 %x_0, 3
  br i1 %1, label %if.exit_0, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_0
  %x_0.tr2 = phi i32 [ %4, %if.else_0 ], [ %x_0, %0 ]
  %accumulator.tr1 = phi i32 [ %5, %if.else_0 ], [ 0, %0 ]
  %2 = add i32 %x_0.tr2, -1
  %3 = tail call i32 @fib(i32 %2)
  %4 = add i32 %x_0.tr2, -2
  %5 = add i32 %3, %accumulator.tr1
  %6 = icmp ult i32 %4, 3
  br i1 %6, label %if.exit_0.loopexit, label %if.else_0

if.exit_0.loopexit:                               ; preds = %if.else_0
  %phi.bo = add i32 %5, 1
  br label %if.exit_0

if.exit_0:                                        ; preds = %if.exit_0.loopexit, %0
  %accumulator.tr.lcssa = phi i32 [ 1, %0 ], [ %phi.bo, %if.exit_0.loopexit ]
  ret i32 %accumulator.tr.lcssa
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i32 @fib(i32 6)
  %2 = tail call i32 @printi(i32 %1)
  ret i32 %2
}

attributes #0 = { norecurse nounwind readonly willreturn }
attributes #1 = { nounwind readnone }


8
