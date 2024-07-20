Optimized LLVM assembly:
; ModuleID = 'kaleidoscope'
source_filename = "<string>"

%IntList = type { i32, %IntList* }

@VoidIntList = local_unnamed_addr constant %IntList* null
@a = local_unnamed_addr global { i32, i32 } { i32 9, i32 10 }

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

define { i32, i32 } @main() local_unnamed_addr {
  %.unpack = load i32, i32* getelementptr inbounds ({ i32, i32 }, { i32, i32 }* @a, i64 0, i32 0), align 8
  %1 = insertvalue { i32, i32 } undef, i32 %.unpack, 0
  %.unpack1 = load i32, i32* getelementptr inbounds ({ i32, i32 }, { i32, i32 }* @a, i64 0, i32 1), align 4
  %2 = insertvalue { i32, i32 } %1, i32 %.unpack1, 1
  %3 = tail call i32 @print_tuple({ i32, i32 } %2, i32 1, i32 1)
  ret i32 %3
}

attributes #0 = { norecurse nounwind readonly willreturn }


0)
