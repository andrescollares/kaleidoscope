Optimized LLVM assembly:
; ModuleID = 'kaleidoscope'
source_filename = "<string>"

%IntList = type { i32, %IntList* }

@VoidIntList = local_unnamed_addr constant %IntList* null

declare i1 @printb(i1) local_unnamed_addr

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

define i1 @main() local_unnamed_addr {
  %.sroa.1.sroa.0 = alloca i8, align 4
  %.sroa.1.sroa.0.0.sroa_cast = bitcast i8* %.sroa.1.sroa.0 to i1*
  store i1 false, i1* %.sroa.1.sroa.0.0.sroa_cast, align 4
  %.sroa.1.sroa.0.0..sroa.1.sroa.0.0..sroa.1.sroa.0.0..sroa.1.4. = load i8, i8* %.sroa.1.sroa.0, align 4
  %.sroa.1.4.insert.ext = zext i8 %.sroa.1.sroa.0.0..sroa.1.sroa.0.0..sroa.1.sroa.0.0..sroa.1.4. to i32
  %1 = tail call i1 @printb(i32 %.sroa.1.4.insert.ext)
  ret i1 %1
}

attributes #0 = { norecurse nounwind readonly willreturn }


false
