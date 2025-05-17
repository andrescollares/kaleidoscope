; ModuleID = 'kaleidoscope'
source_filename = "<string>"

%IntList = type { i32, %IntList* }

@fmtStr = unnamed_addr constant [4 x i8] c"%d\0A\00"

; Function Attrs: optnone
declare i32 @printf(i8*, ...) local_unnamed_addr #0

declare %IntList* @_alloc_int_list_node() local_unnamed_addr

; Function Attrs: norecurse nounwind readnone willreturn
define double @int_to_double(i32 %x_0) local_unnamed_addr #1 {
  %1 = sitofp i32 %x_0 to double
  ret double %1
}

; Function Attrs: norecurse nounwind readnone willreturn
define i32 @double_to_int(double %x_0) local_unnamed_addr #1 {
  %1 = fptosi double %x_0 to i32
  ret i32 %1
}

; Function Attrs: nounwind readonly
define i32 @length.1(%IntList* readonly %l_0) local_unnamed_addr #2 {
  %1 = icmp eq %IntList* %l_0, null
  br i1 %1, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_0
  %l_0.tr2 = phi %IntList* [ %3, %if.else_0 ], [ %l_0, %0 ]
  %accumulator.tr1 = phi i32 [ %4, %if.else_0 ], [ 0, %0 ]
  %2 = getelementptr %IntList, %IntList* %l_0.tr2, i64 0, i32 1
  %3 = load %IntList*, %IntList** %2, align 8
  %4 = add i32 %accumulator.tr1, 1
  %5 = icmp eq %IntList* %3, null
  br i1 %5, label %if.end_0, label %if.else_0

if.end_0:                                         ; preds = %if.else_0, %0
  %accumulator.tr.lcssa = phi i32 [ 0, %0 ], [ %4, %if.else_0 ]
  ret i32 %accumulator.tr.lcssa
}

define i1 @all.2(%IntList* readonly %l_0, i1 (i32)* nocapture %f_0) local_unnamed_addr {
  %1 = icmp eq %IntList* %l_0, null
  br i1 %1, label %if.end_1, label %if.else_0

if.else_0:                                        ; preds = %0, %if.then_1
  %l_0.tr1 = phi %IntList* [ %6, %if.then_1 ], [ %l_0, %0 ]
  %2 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 0
  %3 = load i32, i32* %2, align 4
  %4 = tail call i1 %f_0(i32 %3)
  br i1 %4, label %if.then_1, label %if.end_1

if.then_1:                                        ; preds = %if.else_0
  %5 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 1
  %6 = load %IntList*, %IntList** %5, align 8
  %7 = icmp eq %IntList* %6, null
  br i1 %7, label %if.end_1, label %if.else_0

if.end_1:                                         ; preds = %if.then_1, %if.else_0, %0
  %8 = phi i1 [ true, %0 ], [ %4, %if.else_0 ], [ %4, %if.then_1 ]
  ret i1 %8
}

define i1 @any.3(%IntList* readonly %l_0, i1 (i32)* nocapture %f_0) local_unnamed_addr {
  %1 = icmp eq %IntList* %l_0, null
  br i1 %1, label %if.end_1, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_1
  %l_0.tr1 = phi %IntList* [ %6, %if.else_1 ], [ %l_0, %0 ]
  %2 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 0
  %3 = load i32, i32* %2, align 4
  %4 = tail call i1 %f_0(i32 %3)
  br i1 %4, label %if.end_1, label %if.else_1

if.else_1:                                        ; preds = %if.else_0
  %5 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 1
  %6 = load %IntList*, %IntList** %5, align 8
  %7 = icmp eq %IntList* %6, null
  br i1 %7, label %if.end_1, label %if.else_0

if.end_1:                                         ; preds = %if.else_1, %if.else_0, %0
  %8 = phi i1 [ false, %0 ], [ %4, %if.else_0 ], [ %4, %if.else_1 ]
  ret i1 %8
}

define %IntList* @map_int.4(%IntList* readonly %l_0, i32 (i32)* nocapture %f_0) local_unnamed_addr {
  %1 = icmp eq %IntList* %l_0, null
  br i1 %1, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0
  %2 = getelementptr %IntList, %IntList* %l_0, i64 0, i32 0
  %3 = load i32, i32* %2, align 4
  %4 = tail call i32 %f_0(i32 %3)
  %5 = getelementptr %IntList, %IntList* %l_0, i64 0, i32 1
  %6 = load %IntList*, %IntList** %5, align 8
  %7 = tail call %IntList* @map_int.4(%IntList* %6, i32 (i32)* %f_0)
  %8 = tail call %IntList* @_alloc_int_list_node()
  %9 = getelementptr %IntList, %IntList* %8, i64 0, i32 0
  store i32 %4, i32* %9, align 4
  %10 = getelementptr %IntList, %IntList* %8, i64 0, i32 1
  store %IntList* %7, %IntList** %10, align 8
  ret %IntList* %8

if.end_0:                                         ; preds = %0
  ret %IntList* null
}

define %IntList* @filter_int.5(%IntList* readonly %l_0, i1 (i32)* %f_0) local_unnamed_addr {
  %1 = icmp eq %IntList* %l_0, null
  br i1 %1, label %if.end_1, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_1
  %l_0.tr2 = phi %IntList* [ %14, %if.else_1 ], [ %l_0, %0 ]
  %2 = getelementptr %IntList, %IntList* %l_0.tr2, i64 0, i32 0
  %3 = load i32, i32* %2, align 4
  %4 = tail call i1 %f_0(i32 %3)
  br i1 %4, label %if.then_1, label %if.else_1

if.then_1:                                        ; preds = %if.else_0
  %5 = getelementptr %IntList, %IntList* %l_0.tr2, i64 0, i32 0
  %6 = load i32, i32* %5, align 4
  %7 = getelementptr %IntList, %IntList* %l_0.tr2, i64 0, i32 1
  %8 = load %IntList*, %IntList** %7, align 8
  %9 = tail call %IntList* @filter_int.5(%IntList* %8, i1 (i32)* %f_0)
  %10 = tail call %IntList* @_alloc_int_list_node()
  %11 = getelementptr %IntList, %IntList* %10, i64 0, i32 0
  store i32 %6, i32* %11, align 4
  %12 = getelementptr %IntList, %IntList* %10, i64 0, i32 1
  store %IntList* %9, %IntList** %12, align 8
  ret %IntList* %10

if.else_1:                                        ; preds = %if.else_0
  %13 = getelementptr %IntList, %IntList* %l_0.tr2, i64 0, i32 1
  %14 = load %IntList*, %IntList** %13, align 8
  %15 = icmp eq %IntList* %14, null
  br i1 %15, label %if.end_1, label %if.else_0

if.end_1:                                         ; preds = %if.else_1, %0
  ret %IntList* null
}

; Function Attrs: nounwind readonly
define i32 @nth_int.6(%IntList* nocapture readonly %l_0, i32 %n_0) local_unnamed_addr #2 {
  %1 = icmp eq i32 %n_0, 0
  br i1 %1, label %if.if_exit_0, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_0
  %n_0.tr2 = phi i32 [ %4, %if.else_0 ], [ %n_0, %0 ]
  %l_0.tr1 = phi %IntList* [ %3, %if.else_0 ], [ %l_0, %0 ]
  %2 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 1
  %3 = load %IntList*, %IntList** %2, align 8
  %4 = add i32 %n_0.tr2, -1
  %5 = icmp eq i32 %4, 0
  br i1 %5, label %if.if_exit_0, label %if.else_0

if.if_exit_0:                                     ; preds = %if.else_0, %0
  %l_0.tr.lcssa = phi %IntList* [ %l_0, %0 ], [ %3, %if.else_0 ]
  %6 = getelementptr %IntList, %IntList* %l_0.tr.lcssa, i64 0, i32 0
  %7 = load i32, i32* %6, align 4
  ret i32 %7
}

define %IntList* @reverse_acc_int.7(%IntList* readonly %l_0, %IntList* %acc_0) local_unnamed_addr {
  %1 = icmp eq %IntList* %l_0, null
  br i1 %1, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_0
  %acc_0.tr2 = phi %IntList* [ %6, %if.else_0 ], [ %acc_0, %0 ]
  %l_0.tr1 = phi %IntList* [ %3, %if.else_0 ], [ %l_0, %0 ]
  %2 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 1
  %3 = load %IntList*, %IntList** %2, align 8
  %4 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 0
  %5 = load i32, i32* %4, align 4
  %6 = tail call %IntList* @_alloc_int_list_node()
  %7 = getelementptr %IntList, %IntList* %6, i64 0, i32 0
  store i32 %5, i32* %7, align 4
  %8 = getelementptr %IntList, %IntList* %6, i64 0, i32 1
  store %IntList* %acc_0.tr2, %IntList** %8, align 8
  %9 = icmp eq %IntList* %3, null
  br i1 %9, label %if.end_0, label %if.else_0

if.end_0:                                         ; preds = %if.else_0, %0
  %acc_0.tr.lcssa = phi %IntList* [ %acc_0, %0 ], [ %6, %if.else_0 ]
  ret %IntList* %acc_0.tr.lcssa
}

define %IntList* @reverse_int.8(%IntList* readonly %l_0) local_unnamed_addr {
  %1 = tail call %IntList* @reverse_acc_int.7(%IntList* %l_0, %IntList* null)
  ret %IntList* %1
}

define i32 @foldl_int.9(%IntList* readonly %l_0, i32 %acc_0, i32 (i32, i32)* nocapture %f_0) local_unnamed_addr {
  %1 = icmp eq %IntList* %l_0, null
  br i1 %1, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_0
  %acc_0.tr2 = phi i32 [ %6, %if.else_0 ], [ %acc_0, %0 ]
  %l_0.tr1 = phi %IntList* [ %3, %if.else_0 ], [ %l_0, %0 ]
  %2 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 1
  %3 = load %IntList*, %IntList** %2, align 8
  %4 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 0
  %5 = load i32, i32* %4, align 4
  %6 = tail call i32 %f_0(i32 %acc_0.tr2, i32 %5)
  %7 = icmp eq %IntList* %3, null
  br i1 %7, label %if.end_0, label %if.else_0

if.end_0:                                         ; preds = %if.else_0, %0
  %acc_0.tr.lcssa = phi i32 [ %acc_0, %0 ], [ %6, %if.else_0 ]
  ret i32 %acc_0.tr.lcssa
}

define i32 @foldr_int.10(%IntList* readonly %l_0, i32 %acc_0, i32 (i32, i32)* nocapture %f_0) local_unnamed_addr {
  %1 = icmp eq %IntList* %l_0, null
  br i1 %1, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0
  %2 = getelementptr %IntList, %IntList* %l_0, i64 0, i32 0
  %3 = load i32, i32* %2, align 4
  %4 = getelementptr %IntList, %IntList* %l_0, i64 0, i32 1
  %5 = load %IntList*, %IntList** %4, align 8
  %6 = tail call i32 @foldr_int.10(%IntList* %5, i32 %acc_0, i32 (i32, i32)* %f_0)
  %7 = tail call i32 %f_0(i32 %3, i32 %6)
  ret i32 %7

if.end_0:                                         ; preds = %0
  ret i32 %acc_0
}

; Function Attrs: norecurse nounwind readnone willreturn
define i32 @add.11(i32 %a_0, i32 %b_0) local_unnamed_addr #1 {
  %1 = add i32 %b_0, %a_0
  ret i32 %1
}

define i32 @main() local_unnamed_addr {
  %1 = tail call i32 @add.11(i32 1, i32 2)
  %2 = tail call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([4 x i8], [4 x i8]* @fmtStr, i64 0, i64 0), i32 %1)
  ret i32 0
}

attributes #0 = { optnone }
attributes #1 = { norecurse nounwind readnone willreturn }
attributes #2 = { nounwind readonly }
