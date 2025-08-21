; ModuleID = 'lambdakal'
source_filename = "<string>"

%IntList = type { i32, %IntList* }
%FloatList = type { double, %FloatList* }

@fmtStr = unnamed_addr constant [4 x i8] c"%f\0A\00"

; Function Attrs: optnone
declare i32 @printf(i8*, ...) local_unnamed_addr #0

declare %IntList* @_alloc_int_list_node() local_unnamed_addr

declare %FloatList* @_alloc_double_list_node() local_unnamed_addr

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
define i32 @length_double.1(%FloatList* %l_0) local_unnamed_addr #2 {
  %1 = ptrtoint %FloatList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_0
  %l_0.tr2 = phi %FloatList* [ %5, %if.else_0 ], [ %l_0, %0 ]
  %accumulator.tr1 = phi i32 [ %6, %if.else_0 ], [ 0, %0 ]
  %4 = getelementptr %FloatList, %FloatList* %l_0.tr2, i64 0, i32 1
  %5 = load %FloatList*, %FloatList** %4, align 8
  %6 = add i32 %accumulator.tr1, 1
  %7 = ptrtoint %FloatList* %5 to i64
  %8 = trunc i64 %7 to i32
  %9 = icmp eq i32 %8, 0
  br i1 %9, label %if.end_0, label %if.else_0

if.end_0:                                         ; preds = %if.else_0, %0
  %accumulator.tr.lcssa = phi i32 [ 0, %0 ], [ %6, %if.else_0 ]
  ret i32 %accumulator.tr.lcssa
}

define i1 @all_double.2(%FloatList* %l_0, i1 (double)* nocapture %f_0) local_unnamed_addr {
  %1 = ptrtoint %FloatList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_1, label %if.else_0

if.else_0:                                        ; preds = %0, %if.then_1
  %l_0.tr1 = phi %FloatList* [ %8, %if.then_1 ], [ %l_0, %0 ]
  %4 = getelementptr %FloatList, %FloatList* %l_0.tr1, i64 0, i32 0
  %5 = load double, double* %4, align 8
  %6 = tail call i1 %f_0(double %5)
  br i1 %6, label %if.then_1, label %if.end_1

if.then_1:                                        ; preds = %if.else_0
  %7 = getelementptr %FloatList, %FloatList* %l_0.tr1, i64 0, i32 1
  %8 = load %FloatList*, %FloatList** %7, align 8
  %9 = ptrtoint %FloatList* %8 to i64
  %10 = trunc i64 %9 to i32
  %11 = icmp eq i32 %10, 0
  br i1 %11, label %if.end_1, label %if.else_0

if.end_1:                                         ; preds = %if.then_1, %if.else_0, %0
  %12 = phi i1 [ true, %0 ], [ %6, %if.else_0 ], [ %6, %if.then_1 ]
  ret i1 %12
}

define i1 @any_double.3(%FloatList* %l_0, i1 (double)* nocapture %f_0) local_unnamed_addr {
  %1 = ptrtoint %FloatList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_1, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_1
  %l_0.tr1 = phi %FloatList* [ %8, %if.else_1 ], [ %l_0, %0 ]
  %4 = getelementptr %FloatList, %FloatList* %l_0.tr1, i64 0, i32 0
  %5 = load double, double* %4, align 8
  %6 = tail call i1 %f_0(double %5)
  br i1 %6, label %if.end_1, label %if.else_1

if.else_1:                                        ; preds = %if.else_0
  %7 = getelementptr %FloatList, %FloatList* %l_0.tr1, i64 0, i32 1
  %8 = load %FloatList*, %FloatList** %7, align 8
  %9 = ptrtoint %FloatList* %8 to i64
  %10 = trunc i64 %9 to i32
  %11 = icmp eq i32 %10, 0
  br i1 %11, label %if.end_1, label %if.else_0

if.end_1:                                         ; preds = %if.else_1, %if.else_0, %0
  %12 = phi i1 [ false, %0 ], [ %6, %if.else_0 ], [ %6, %if.else_1 ]
  ret i1 %12
}

define %FloatList* @map_double.4(%FloatList* %l_0, double (double)* nocapture %f_0) local_unnamed_addr {
  %1 = ptrtoint %FloatList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0
  %4 = getelementptr %FloatList, %FloatList* %l_0, i64 0, i32 0
  %5 = load double, double* %4, align 8
  %6 = tail call double %f_0(double %5)
  %7 = getelementptr %FloatList, %FloatList* %l_0, i64 0, i32 1
  %8 = load %FloatList*, %FloatList** %7, align 8
  %9 = tail call %FloatList* @map_double.4(%FloatList* %8, double (double)* %f_0)
  %10 = tail call %FloatList* @_alloc_double_list_node()
  %11 = getelementptr %FloatList, %FloatList* %10, i64 0, i32 0
  store double %6, double* %11, align 8
  %12 = getelementptr %FloatList, %FloatList* %10, i64 0, i32 1
  store %FloatList* %9, %FloatList** %12, align 8
  ret %FloatList* %10

if.end_0:                                         ; preds = %0
  ret %FloatList* null
}

define %FloatList* @filter_double.5(%FloatList* %l_0, i1 (double)* %f_0) local_unnamed_addr {
  %1 = ptrtoint %FloatList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_1, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_1
  %l_0.tr2 = phi %FloatList* [ %16, %if.else_1 ], [ %l_0, %0 ]
  %4 = getelementptr %FloatList, %FloatList* %l_0.tr2, i64 0, i32 0
  %5 = load double, double* %4, align 8
  %6 = tail call i1 %f_0(double %5)
  br i1 %6, label %if.then_1, label %if.else_1

if.then_1:                                        ; preds = %if.else_0
  %7 = getelementptr %FloatList, %FloatList* %l_0.tr2, i64 0, i32 0
  %8 = load double, double* %7, align 8
  %9 = getelementptr %FloatList, %FloatList* %l_0.tr2, i64 0, i32 1
  %10 = load %FloatList*, %FloatList** %9, align 8
  %11 = tail call %FloatList* @filter_double.5(%FloatList* %10, i1 (double)* %f_0)
  %12 = tail call %FloatList* @_alloc_double_list_node()
  %13 = getelementptr %FloatList, %FloatList* %12, i64 0, i32 0
  store double %8, double* %13, align 8
  %14 = getelementptr %FloatList, %FloatList* %12, i64 0, i32 1
  store %FloatList* %11, %FloatList** %14, align 8
  ret %FloatList* %12

if.else_1:                                        ; preds = %if.else_0
  %15 = getelementptr %FloatList, %FloatList* %l_0.tr2, i64 0, i32 1
  %16 = load %FloatList*, %FloatList** %15, align 8
  %17 = ptrtoint %FloatList* %16 to i64
  %18 = trunc i64 %17 to i32
  %19 = icmp eq i32 %18, 0
  br i1 %19, label %if.end_1, label %if.else_0

if.end_1:                                         ; preds = %if.else_1, %0
  ret %FloatList* null
}

; Function Attrs: nounwind readonly
define double @nth_double.6(%FloatList* nocapture readonly %l_0, i32 %n_0) local_unnamed_addr #2 {
  %1 = icmp eq i32 %n_0, 0
  br i1 %1, label %if.if_exit_0, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_0
  %n_0.tr2 = phi i32 [ %4, %if.else_0 ], [ %n_0, %0 ]
  %l_0.tr1 = phi %FloatList* [ %3, %if.else_0 ], [ %l_0, %0 ]
  %2 = getelementptr %FloatList, %FloatList* %l_0.tr1, i64 0, i32 1
  %3 = load %FloatList*, %FloatList** %2, align 8
  %4 = add i32 %n_0.tr2, -1
  %5 = icmp eq i32 %4, 0
  br i1 %5, label %if.if_exit_0, label %if.else_0

if.if_exit_0:                                     ; preds = %if.else_0, %0
  %l_0.tr.lcssa = phi %FloatList* [ %l_0, %0 ], [ %3, %if.else_0 ]
  %6 = getelementptr %FloatList, %FloatList* %l_0.tr.lcssa, i64 0, i32 0
  %7 = load double, double* %6, align 8
  ret double %7
}

define %FloatList* @reverse_acc_double.7(%FloatList* %l_0, %FloatList* %acc_0) local_unnamed_addr {
  %1 = ptrtoint %FloatList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_0
  %acc_0.tr2 = phi %FloatList* [ %8, %if.else_0 ], [ %acc_0, %0 ]
  %l_0.tr1 = phi %FloatList* [ %5, %if.else_0 ], [ %l_0, %0 ]
  %4 = getelementptr %FloatList, %FloatList* %l_0.tr1, i64 0, i32 1
  %5 = load %FloatList*, %FloatList** %4, align 8
  %6 = getelementptr %FloatList, %FloatList* %l_0.tr1, i64 0, i32 0
  %7 = load double, double* %6, align 8
  %8 = tail call %FloatList* @_alloc_double_list_node()
  %9 = getelementptr %FloatList, %FloatList* %8, i64 0, i32 0
  store double %7, double* %9, align 8
  %10 = getelementptr %FloatList, %FloatList* %8, i64 0, i32 1
  store %FloatList* %acc_0.tr2, %FloatList** %10, align 8
  %11 = ptrtoint %FloatList* %5 to i64
  %12 = trunc i64 %11 to i32
  %13 = icmp eq i32 %12, 0
  br i1 %13, label %if.end_0, label %if.else_0

if.end_0:                                         ; preds = %if.else_0, %0
  %acc_0.tr.lcssa = phi %FloatList* [ %acc_0, %0 ], [ %8, %if.else_0 ]
  ret %FloatList* %acc_0.tr.lcssa
}

define %FloatList* @reverse_double.8(%FloatList* %l_0) local_unnamed_addr {
  %1 = tail call %FloatList* @reverse_acc_double.7(%FloatList* %l_0, %FloatList* null)
  ret %FloatList* %1
}

define double @foldl_double.9(%FloatList* %l_0, double %acc_0, double (double, double)* nocapture %f_0) local_unnamed_addr {
  %1 = ptrtoint %FloatList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_0
  %acc_0.tr2 = phi double [ %8, %if.else_0 ], [ %acc_0, %0 ]
  %l_0.tr1 = phi %FloatList* [ %5, %if.else_0 ], [ %l_0, %0 ]
  %4 = getelementptr %FloatList, %FloatList* %l_0.tr1, i64 0, i32 1
  %5 = load %FloatList*, %FloatList** %4, align 8
  %6 = getelementptr %FloatList, %FloatList* %l_0.tr1, i64 0, i32 0
  %7 = load double, double* %6, align 8
  %8 = tail call double %f_0(double %acc_0.tr2, double %7)
  %9 = ptrtoint %FloatList* %5 to i64
  %10 = trunc i64 %9 to i32
  %11 = icmp eq i32 %10, 0
  br i1 %11, label %if.end_0, label %if.else_0

if.end_0:                                         ; preds = %if.else_0, %0
  %acc_0.tr.lcssa = phi double [ %acc_0, %0 ], [ %8, %if.else_0 ]
  ret double %acc_0.tr.lcssa
}

define double @foldr_double.10(%FloatList* %l_0, double %acc_0, double (double, double)* nocapture %f_0) local_unnamed_addr {
  %1 = ptrtoint %FloatList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0
  %4 = getelementptr %FloatList, %FloatList* %l_0, i64 0, i32 0
  %5 = load double, double* %4, align 8
  %6 = getelementptr %FloatList, %FloatList* %l_0, i64 0, i32 1
  %7 = load %FloatList*, %FloatList** %6, align 8
  %8 = tail call double @foldr_double.10(%FloatList* %7, double %acc_0, double (double, double)* %f_0)
  %9 = tail call double %f_0(double %5, double %8)
  ret double %9

if.end_0:                                         ; preds = %0
  ret double %acc_0
}

; Function Attrs: nounwind readonly
define i32 @min_index_double.11(%FloatList* %arr_0, i32 %start_0) local_unnamed_addr #2 {
  %1 = tail call i32 @length_double.1(%FloatList* %arr_0)
  %2 = add i32 %1, -1
  %.not = icmp sgt i32 %2, %start_0
  br i1 %.not, label %if.else_0, label %if.end_1

if.else_0:                                        ; preds = %0
  %3 = tail call double @nth_double.6(%FloatList* %arr_0, i32 %start_0)
  %4 = add nsw i32 %start_0, 1
  %5 = tail call i32 @min_index_double.11(%FloatList* %arr_0, i32 %4)
  %6 = tail call double @nth_double.6(%FloatList* %arr_0, i32 %5)
  %7 = fcmp ult double %3, %6
  br i1 %7, label %if.end_1, label %if.else_1

if.else_1:                                        ; preds = %if.else_0
  br label %if.end_1

if.end_1:                                         ; preds = %if.else_0, %if.else_1, %0
  %8 = phi i32 [ %start_0, %0 ], [ %5, %if.else_1 ], [ %start_0, %if.else_0 ]
  ret i32 %8
}

; Function Attrs: nofree nounwind
define nonnull { double, i32 }* @_internal_find_min_double.12(%FloatList* %arr_0, double %min_val_0, i32 %min_idx_0, i32 %acc_0) local_unnamed_addr #3 {
  %1 = ptrtoint %FloatList* %arr_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  %4 = alloca { double, i32 }, align 8
  %5 = getelementptr inbounds { double, i32 }, { double, i32 }* %4, i64 0, i32 0
  store volatile double %min_val_0, double* %5, align 8
  %6 = getelementptr inbounds { double, i32 }, { double, i32 }* %4, i64 0, i32 1
  store volatile i32 %min_idx_0, i32* %6, align 8
  br label %if.end_1

if.else_0:                                        ; preds = %0
  %7 = getelementptr %FloatList, %FloatList* %arr_0, i64 0, i32 0
  %8 = load double, double* %7, align 8
  %9 = fcmp ult double %8, %min_val_0
  %10 = getelementptr %FloatList, %FloatList* %arr_0, i64 0, i32 1
  %11 = load %FloatList*, %FloatList** %10, align 8
  %12 = add i32 %acc_0, 1
  br i1 %9, label %if.then_1, label %if.else_1

if.then_1:                                        ; preds = %if.else_0
  %13 = tail call { double, i32 }* @_internal_find_min_double.12(%FloatList* %11, double %8, i32 %acc_0, i32 %12)
  br label %if.end_1

if.else_1:                                        ; preds = %if.else_0
  %14 = tail call { double, i32 }* @_internal_find_min_double.12(%FloatList* %11, double %min_val_0, i32 %min_idx_0, i32 %12)
  br label %if.end_1

if.end_1:                                         ; preds = %if.else_1, %if.then_1, %if.then_0
  %15 = phi { double, i32 }* [ %4, %if.then_0 ], [ %13, %if.then_1 ], [ %14, %if.else_1 ]
  ret { double, i32 }* %15
}

; Function Attrs: nofree nounwind
define nonnull { double, i32 }* @find_min_double.13(%FloatList* %arr_0, double %min_val_0) local_unnamed_addr #3 {
  %1 = tail call { double, i32 }* @_internal_find_min_double.12(%FloatList* %arr_0, double %min_val_0, i32 0, i32 0)
  ret { double, i32 }* %1
}

define %FloatList* @remove_nth_double.14(%FloatList* nocapture readonly %arr_0, i32 %n_0) local_unnamed_addr {
  %1 = icmp eq i32 %n_0, 0
  br i1 %1, label %if.if_exit_0, label %if.else_0

if.else_0:                                        ; preds = %0
  %2 = getelementptr %FloatList, %FloatList* %arr_0, i64 0, i32 0
  %3 = load double, double* %2, align 8
  %4 = getelementptr %FloatList, %FloatList* %arr_0, i64 0, i32 1
  %5 = load %FloatList*, %FloatList** %4, align 8
  %6 = add i32 %n_0, -1
  %7 = tail call %FloatList* @remove_nth_double.14(%FloatList* %5, i32 %6)
  %8 = tail call %FloatList* @_alloc_double_list_node()
  %9 = getelementptr %FloatList, %FloatList* %8, i64 0, i32 0
  store double %3, double* %9, align 8
  %10 = getelementptr %FloatList, %FloatList* %8, i64 0, i32 1
  store %FloatList* %7, %FloatList** %10, align 8
  ret %FloatList* %8

if.if_exit_0:                                     ; preds = %0
  %11 = getelementptr %FloatList, %FloatList* %arr_0, i64 0, i32 1
  %12 = load %FloatList*, %FloatList** %11, align 8
  ret %FloatList* %12
}

define %FloatList* @selection_sort_double.15(%FloatList* %arr_0) local_unnamed_addr {
  %1 = ptrtoint %FloatList* %arr_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0
  %4 = tail call { double, i32 }* @find_min_double.13(%FloatList* %arr_0, double 9.999990e+05)
  %5 = getelementptr inbounds { double, i32 }, { double, i32 }* %4, i64 0, i32 0
  %6 = load double, double* %5, align 8
  %7 = getelementptr inbounds { double, i32 }, { double, i32 }* %4, i64 0, i32 1
  %8 = load i32, i32* %7, align 4
  %9 = tail call %FloatList* @remove_nth_double.14(%FloatList* %arr_0, i32 %8)
  %10 = tail call %FloatList* @selection_sort_double.15(%FloatList* %9)
  %11 = tail call %FloatList* @_alloc_double_list_node()
  %12 = getelementptr %FloatList, %FloatList* %11, i64 0, i32 0
  store double %6, double* %12, align 8
  %13 = getelementptr %FloatList, %FloatList* %11, i64 0, i32 1
  store %FloatList* %10, %FloatList** %13, align 8
  ret %FloatList* %11

if.end_0:                                         ; preds = %0
  ret %FloatList* null
}

; Function Attrs: nounwind readonly
define i32 @length.16(%IntList* %l_0) local_unnamed_addr #2 {
  %1 = ptrtoint %IntList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_0
  %l_0.tr2 = phi %IntList* [ %5, %if.else_0 ], [ %l_0, %0 ]
  %accumulator.tr1 = phi i32 [ %6, %if.else_0 ], [ 0, %0 ]
  %4 = getelementptr %IntList, %IntList* %l_0.tr2, i64 0, i32 1
  %5 = load %IntList*, %IntList** %4, align 8
  %6 = add i32 %accumulator.tr1, 1
  %7 = ptrtoint %IntList* %5 to i64
  %8 = trunc i64 %7 to i32
  %9 = icmp eq i32 %8, 0
  br i1 %9, label %if.end_0, label %if.else_0

if.end_0:                                         ; preds = %if.else_0, %0
  %accumulator.tr.lcssa = phi i32 [ 0, %0 ], [ %6, %if.else_0 ]
  ret i32 %accumulator.tr.lcssa
}

define i1 @all.17(%IntList* %l_0, i1 (i32)* nocapture %f_0) local_unnamed_addr {
  %1 = ptrtoint %IntList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_1, label %if.else_0

if.else_0:                                        ; preds = %0, %if.then_1
  %l_0.tr1 = phi %IntList* [ %8, %if.then_1 ], [ %l_0, %0 ]
  %4 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 0
  %5 = load i32, i32* %4, align 4
  %6 = tail call i1 %f_0(i32 %5)
  br i1 %6, label %if.then_1, label %if.end_1

if.then_1:                                        ; preds = %if.else_0
  %7 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 1
  %8 = load %IntList*, %IntList** %7, align 8
  %9 = ptrtoint %IntList* %8 to i64
  %10 = trunc i64 %9 to i32
  %11 = icmp eq i32 %10, 0
  br i1 %11, label %if.end_1, label %if.else_0

if.end_1:                                         ; preds = %if.then_1, %if.else_0, %0
  %12 = phi i1 [ true, %0 ], [ %6, %if.else_0 ], [ %6, %if.then_1 ]
  ret i1 %12
}

define i1 @any.18(%IntList* %l_0, i1 (i32)* nocapture %f_0) local_unnamed_addr {
  %1 = ptrtoint %IntList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_1, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_1
  %l_0.tr1 = phi %IntList* [ %8, %if.else_1 ], [ %l_0, %0 ]
  %4 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 0
  %5 = load i32, i32* %4, align 4
  %6 = tail call i1 %f_0(i32 %5)
  br i1 %6, label %if.end_1, label %if.else_1

if.else_1:                                        ; preds = %if.else_0
  %7 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 1
  %8 = load %IntList*, %IntList** %7, align 8
  %9 = ptrtoint %IntList* %8 to i64
  %10 = trunc i64 %9 to i32
  %11 = icmp eq i32 %10, 0
  br i1 %11, label %if.end_1, label %if.else_0

if.end_1:                                         ; preds = %if.else_1, %if.else_0, %0
  %12 = phi i1 [ false, %0 ], [ %6, %if.else_0 ], [ %6, %if.else_1 ]
  ret i1 %12
}

define %IntList* @map_int.19(%IntList* %l_0, i32 (i32)* nocapture %f_0) local_unnamed_addr {
  %1 = ptrtoint %IntList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0
  %4 = getelementptr %IntList, %IntList* %l_0, i64 0, i32 0
  %5 = load i32, i32* %4, align 4
  %6 = tail call i32 %f_0(i32 %5)
  %7 = getelementptr %IntList, %IntList* %l_0, i64 0, i32 1
  %8 = load %IntList*, %IntList** %7, align 8
  %9 = tail call %IntList* @map_int.19(%IntList* %8, i32 (i32)* %f_0)
  %10 = tail call %IntList* @_alloc_int_list_node()
  %11 = getelementptr %IntList, %IntList* %10, i64 0, i32 0
  store i32 %6, i32* %11, align 4
  %12 = getelementptr %IntList, %IntList* %10, i64 0, i32 1
  store %IntList* %9, %IntList** %12, align 8
  ret %IntList* %10

if.end_0:                                         ; preds = %0
  ret %IntList* null
}

define %IntList* @filter_int.20(%IntList* %l_0, i1 (i32)* %f_0) local_unnamed_addr {
  %1 = ptrtoint %IntList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_1, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_1
  %l_0.tr2 = phi %IntList* [ %16, %if.else_1 ], [ %l_0, %0 ]
  %4 = getelementptr %IntList, %IntList* %l_0.tr2, i64 0, i32 0
  %5 = load i32, i32* %4, align 4
  %6 = tail call i1 %f_0(i32 %5)
  br i1 %6, label %if.then_1, label %if.else_1

if.then_1:                                        ; preds = %if.else_0
  %7 = getelementptr %IntList, %IntList* %l_0.tr2, i64 0, i32 0
  %8 = load i32, i32* %7, align 4
  %9 = getelementptr %IntList, %IntList* %l_0.tr2, i64 0, i32 1
  %10 = load %IntList*, %IntList** %9, align 8
  %11 = tail call %IntList* @filter_int.20(%IntList* %10, i1 (i32)* %f_0)
  %12 = tail call %IntList* @_alloc_int_list_node()
  %13 = getelementptr %IntList, %IntList* %12, i64 0, i32 0
  store i32 %8, i32* %13, align 4
  %14 = getelementptr %IntList, %IntList* %12, i64 0, i32 1
  store %IntList* %11, %IntList** %14, align 8
  ret %IntList* %12

if.else_1:                                        ; preds = %if.else_0
  %15 = getelementptr %IntList, %IntList* %l_0.tr2, i64 0, i32 1
  %16 = load %IntList*, %IntList** %15, align 8
  %17 = ptrtoint %IntList* %16 to i64
  %18 = trunc i64 %17 to i32
  %19 = icmp eq i32 %18, 0
  br i1 %19, label %if.end_1, label %if.else_0

if.end_1:                                         ; preds = %if.else_1, %0
  ret %IntList* null
}

; Function Attrs: nounwind readonly
define i32 @nth_int.21(%IntList* nocapture readonly %l_0, i32 %n_0) local_unnamed_addr #2 {
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

define %IntList* @reverse_acc_int.22(%IntList* %l_0, %IntList* %acc_0) local_unnamed_addr {
  %1 = ptrtoint %IntList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_0
  %acc_0.tr2 = phi %IntList* [ %8, %if.else_0 ], [ %acc_0, %0 ]
  %l_0.tr1 = phi %IntList* [ %5, %if.else_0 ], [ %l_0, %0 ]
  %4 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 1
  %5 = load %IntList*, %IntList** %4, align 8
  %6 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 0
  %7 = load i32, i32* %6, align 4
  %8 = tail call %IntList* @_alloc_int_list_node()
  %9 = getelementptr %IntList, %IntList* %8, i64 0, i32 0
  store i32 %7, i32* %9, align 4
  %10 = getelementptr %IntList, %IntList* %8, i64 0, i32 1
  store %IntList* %acc_0.tr2, %IntList** %10, align 8
  %11 = ptrtoint %IntList* %5 to i64
  %12 = trunc i64 %11 to i32
  %13 = icmp eq i32 %12, 0
  br i1 %13, label %if.end_0, label %if.else_0

if.end_0:                                         ; preds = %if.else_0, %0
  %acc_0.tr.lcssa = phi %IntList* [ %acc_0, %0 ], [ %8, %if.else_0 ]
  ret %IntList* %acc_0.tr.lcssa
}

define %IntList* @reverse_int.23(%IntList* %l_0) local_unnamed_addr {
  %1 = tail call %IntList* @reverse_acc_int.22(%IntList* %l_0, %IntList* null)
  ret %IntList* %1
}

define i32 @foldl_int.24(%IntList* %l_0, i32 %acc_0, i32 (i32, i32)* nocapture %f_0) local_unnamed_addr {
  %1 = ptrtoint %IntList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_0
  %acc_0.tr2 = phi i32 [ %8, %if.else_0 ], [ %acc_0, %0 ]
  %l_0.tr1 = phi %IntList* [ %5, %if.else_0 ], [ %l_0, %0 ]
  %4 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 1
  %5 = load %IntList*, %IntList** %4, align 8
  %6 = getelementptr %IntList, %IntList* %l_0.tr1, i64 0, i32 0
  %7 = load i32, i32* %6, align 4
  %8 = tail call i32 %f_0(i32 %acc_0.tr2, i32 %7)
  %9 = ptrtoint %IntList* %5 to i64
  %10 = trunc i64 %9 to i32
  %11 = icmp eq i32 %10, 0
  br i1 %11, label %if.end_0, label %if.else_0

if.end_0:                                         ; preds = %if.else_0, %0
  %acc_0.tr.lcssa = phi i32 [ %acc_0, %0 ], [ %8, %if.else_0 ]
  ret i32 %acc_0.tr.lcssa
}

define i32 @foldr_int.25(%IntList* %l_0, i32 %acc_0, i32 (i32, i32)* nocapture %f_0) local_unnamed_addr {
  %1 = ptrtoint %IntList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0
  %4 = getelementptr %IntList, %IntList* %l_0, i64 0, i32 0
  %5 = load i32, i32* %4, align 4
  %6 = getelementptr %IntList, %IntList* %l_0, i64 0, i32 1
  %7 = load %IntList*, %IntList** %6, align 8
  %8 = tail call i32 @foldr_int.25(%IntList* %7, i32 %acc_0, i32 (i32, i32)* %f_0)
  %9 = tail call i32 %f_0(i32 %5, i32 %8)
  ret i32 %9

if.end_0:                                         ; preds = %0
  ret i32 %acc_0
}

; Function Attrs: nounwind readonly
define i32 @min_index_int.26(%IntList* %arr_0, i32 %start_0) local_unnamed_addr #2 {
  %1 = tail call i32 @length.16(%IntList* %arr_0)
  %2 = add i32 %1, -1
  %.not = icmp sgt i32 %2, %start_0
  br i1 %.not, label %if.else_0, label %if.end_1

if.else_0:                                        ; preds = %0
  %3 = tail call i32 @nth_int.21(%IntList* %arr_0, i32 %start_0)
  %4 = add nsw i32 %start_0, 1
  %5 = tail call i32 @min_index_int.26(%IntList* %arr_0, i32 %4)
  %6 = tail call i32 @nth_int.21(%IntList* %arr_0, i32 %5)
  %7 = icmp slt i32 %3, %6
  %spec.select = select i1 %7, i32 %start_0, i32 %5
  ret i32 %spec.select

if.end_1:                                         ; preds = %0
  ret i32 %start_0
}

; Function Attrs: nofree nounwind
define nonnull { i32, i32 }* @_internal_find_min_int.27(%IntList* %arr_0, i32 %min_val_0, i32 %min_idx_0, i32 %acc_0) local_unnamed_addr #3 {
  %1 = ptrtoint %IntList* %arr_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  %4 = alloca { i32, i32 }, align 8
  %5 = getelementptr inbounds { i32, i32 }, { i32, i32 }* %4, i64 0, i32 0
  store volatile i32 %min_val_0, i32* %5, align 8
  %6 = getelementptr inbounds { i32, i32 }, { i32, i32 }* %4, i64 0, i32 1
  store volatile i32 %min_idx_0, i32* %6, align 4
  br label %if.end_1

if.else_0:                                        ; preds = %0
  %7 = getelementptr %IntList, %IntList* %arr_0, i64 0, i32 0
  %8 = load i32, i32* %7, align 4
  %9 = icmp slt i32 %8, %min_val_0
  %10 = getelementptr %IntList, %IntList* %arr_0, i64 0, i32 1
  %11 = load %IntList*, %IntList** %10, align 8
  %12 = add i32 %acc_0, 1
  br i1 %9, label %if.then_1, label %if.else_1

if.then_1:                                        ; preds = %if.else_0
  %13 = tail call { i32, i32 }* @_internal_find_min_int.27(%IntList* %11, i32 %8, i32 %acc_0, i32 %12)
  br label %if.end_1

if.else_1:                                        ; preds = %if.else_0
  %14 = tail call { i32, i32 }* @_internal_find_min_int.27(%IntList* %11, i32 %min_val_0, i32 %min_idx_0, i32 %12)
  br label %if.end_1

if.end_1:                                         ; preds = %if.else_1, %if.then_1, %if.then_0
  %15 = phi { i32, i32 }* [ %4, %if.then_0 ], [ %13, %if.then_1 ], [ %14, %if.else_1 ]
  ret { i32, i32 }* %15
}

; Function Attrs: nofree nounwind
define nonnull { i32, i32 }* @find_min_int.28(%IntList* %arr_0, i32 %min_val_0) local_unnamed_addr #3 {
  %1 = tail call { i32, i32 }* @_internal_find_min_int.27(%IntList* %arr_0, i32 %min_val_0, i32 0, i32 0)
  ret { i32, i32 }* %1
}

define %IntList* @remove_nth_int.29(%IntList* nocapture readonly %arr_0, i32 %n_0) local_unnamed_addr {
  %1 = icmp eq i32 %n_0, 0
  br i1 %1, label %if.if_exit_0, label %if.else_0

if.else_0:                                        ; preds = %0
  %2 = getelementptr %IntList, %IntList* %arr_0, i64 0, i32 0
  %3 = load i32, i32* %2, align 4
  %4 = getelementptr %IntList, %IntList* %arr_0, i64 0, i32 1
  %5 = load %IntList*, %IntList** %4, align 8
  %6 = add i32 %n_0, -1
  %7 = tail call %IntList* @remove_nth_int.29(%IntList* %5, i32 %6)
  %8 = tail call %IntList* @_alloc_int_list_node()
  %9 = getelementptr %IntList, %IntList* %8, i64 0, i32 0
  store i32 %3, i32* %9, align 4
  %10 = getelementptr %IntList, %IntList* %8, i64 0, i32 1
  store %IntList* %7, %IntList** %10, align 8
  ret %IntList* %8

if.if_exit_0:                                     ; preds = %0
  %11 = getelementptr %IntList, %IntList* %arr_0, i64 0, i32 1
  %12 = load %IntList*, %IntList** %11, align 8
  ret %IntList* %12
}

define %IntList* @selection_sort_int.30(%IntList* %arr_0) local_unnamed_addr {
  %1 = ptrtoint %IntList* %arr_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0
  %4 = tail call { i32, i32 }* @find_min_int.28(%IntList* %arr_0, i32 9999999)
  %.elt = getelementptr inbounds { i32, i32 }, { i32, i32 }* %4, i64 0, i32 0
  %.unpack = load i32, i32* %.elt, align 4
  %.elt5 = getelementptr inbounds { i32, i32 }, { i32, i32 }* %4, i64 0, i32 1
  %.unpack6 = load i32, i32* %.elt5, align 4
  %5 = tail call %IntList* @remove_nth_int.29(%IntList* %arr_0, i32 %.unpack6)
  %6 = tail call %IntList* @selection_sort_int.30(%IntList* %5)
  %7 = tail call %IntList* @_alloc_int_list_node()
  %8 = getelementptr %IntList, %IntList* %7, i64 0, i32 0
  store i32 %.unpack, i32* %8, align 4
  %9 = getelementptr %IntList, %IntList* %7, i64 0, i32 1
  store %IntList* %6, %IntList** %9, align 8
  ret %IntList* %7

if.end_0:                                         ; preds = %0
  ret %IntList* null
}

; Function Attrs: nounwind readonly
define double @sum_acc.31(%FloatList* %l_0, double %acc_0) local_unnamed_addr #2 {
  %1 = ptrtoint %FloatList* %l_0 to i64
  %2 = trunc i64 %1 to i32
  %3 = icmp eq i32 %2, 0
  br i1 %3, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_0
  %acc_0.tr2 = phi double [ %8, %if.else_0 ], [ %acc_0, %0 ]
  %l_0.tr1 = phi %FloatList* [ %5, %if.else_0 ], [ %l_0, %0 ]
  %4 = getelementptr %FloatList, %FloatList* %l_0.tr1, i64 0, i32 1
  %5 = load %FloatList*, %FloatList** %4, align 8
  %6 = getelementptr %FloatList, %FloatList* %l_0.tr1, i64 0, i32 0
  %7 = load double, double* %6, align 8
  %8 = fadd double %acc_0.tr2, %7
  %9 = ptrtoint %FloatList* %5 to i64
  %10 = trunc i64 %9 to i32
  %11 = icmp eq i32 %10, 0
  br i1 %11, label %if.end_0, label %if.else_0

if.end_0:                                         ; preds = %if.else_0, %0
  %acc_0.tr.lcssa = phi double [ %acc_0, %0 ], [ %8, %if.else_0 ]
  ret double %acc_0.tr.lcssa
}

define %FloatList* @one_to_n_acc.32(double %n_0, %FloatList* %acc_0) local_unnamed_addr {
  %1 = fcmp ule double %n_0, 0.000000e+00
  br i1 %1, label %if.end_0, label %if.else_0

if.else_0:                                        ; preds = %0, %if.else_0
  %acc_0.tr2 = phi %FloatList* [ %3, %if.else_0 ], [ %acc_0, %0 ]
  %n_0.tr1 = phi double [ %2, %if.else_0 ], [ %n_0, %0 ]
  %2 = fadd double %n_0.tr1, -1.000000e+00
  %3 = tail call %FloatList* @_alloc_double_list_node()
  %4 = getelementptr %FloatList, %FloatList* %3, i64 0, i32 0
  store double %n_0.tr1, double* %4, align 8
  %5 = getelementptr %FloatList, %FloatList* %3, i64 0, i32 1
  store %FloatList* %acc_0.tr2, %FloatList** %5, align 8
  %6 = fcmp ule double %2, 0.000000e+00
  br i1 %6, label %if.end_0, label %if.else_0

if.end_0:                                         ; preds = %if.else_0, %0
  %acc_0.tr.lcssa = phi %FloatList* [ %acc_0, %0 ], [ %3, %if.else_0 ]
  ret %FloatList* %acc_0.tr.lcssa
}

define i32 @main() local_unnamed_addr {
  %1 = tail call %FloatList* @one_to_n_acc.32(double 1.000000e+04, %FloatList* null)
  %2 = tail call double @sum_acc.31(%FloatList* %1, double 0.000000e+00)
  %3 = tail call i32 (i8*, ...) @printf(i8* nonnull dereferenceable(1) getelementptr inbounds ([4 x i8], [4 x i8]* @fmtStr, i64 0, i64 0), double %2)
  ret i32 0
}

attributes #0 = { optnone }
attributes #1 = { norecurse nounwind readnone willreturn }
attributes #2 = { nounwind readonly }
attributes #3 = { nofree nounwind }
