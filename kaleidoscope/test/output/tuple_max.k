Optimized LLVM assembly:
; ModuleID = 'kaleidoscope'
source_filename = "<string>"

%IntList = type { i32, %IntList* }

@VoidIntList = local_unnamed_addr constant %IntList* null
@VER = local_unnamed_addr global { i32, i32 } { i32 33, i32 1 }

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
define i32 @max({ i32, i32 } %x_0) local_unnamed_addr #1 {
if.exit_0:
  %x_0.elt = extractvalue { i32, i32 } %x_0, 0
  %x_0.elt5 = extractvalue { i32, i32 } %x_0, 1
  %0 = icmp ugt i32 %x_0.elt, %x_0.elt5
  %1 = select i1 %0, i32 %x_0.elt, i32 %x_0.elt5
  ret i32 %1
}

define i32 @main() local_unnamed_addr {
  %.unpack = load i32, i32* getelementptr inbounds ({ i32, i32 }, { i32, i32 }* @VER, i64 0, i32 0), align 8
  %1 = insertvalue { i32, i32 } undef, i32 %.unpack, 0
  %.unpack1 = load i32, i32* getelementptr inbounds ({ i32, i32 }, { i32, i32 }* @VER, i64 0, i32 1), align 4
  %2 = insertvalue { i32, i32 } %1, i32 %.unpack1, 1
  %3 = tail call i32 @max({ i32, i32 } %2)
  %4 = tail call i32 @printi(i32 %3)
  ret i32 %4
}

attributes #0 = { norecurse nounwind readonly willreturn }
attributes #1 = { norecurse nounwind readnone willreturn }


33
