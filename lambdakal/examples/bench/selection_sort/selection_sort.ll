; ModuleID = 'lambdakal'
source_filename = "<string>"

%IntList = type { i32, %IntList* }
%FloatList = type { double, %FloatList* }
%BoolList = type { i1, %BoolList* }

; Function Attrs: optnone
declare i32 @printf(i8*, ...) #0

declare i32 @printil(%IntList*)

declare i32 @printfl(%FloatList*)

declare i32 @printbl(%BoolList*)

declare i8* @intListToString(%IntList*)

declare i8* @floatListToString(%FloatList*)

declare i8* @boolListToString(%BoolList*)

declare double @sin(double)

declare double @cos(double)

declare double @tan(double)

declare double @log(double)

declare double @fabs(double)

declare i32 @rand()

declare i32 @srand(i32)

declare i32 @exit(i32)

declare %IntList* @_alloc_int_list_node()

declare %FloatList* @_alloc_double_list_node()

declare %BoolList* @_alloc_bool_list_node()

define double @int_to_double(i32 %x_0) {
  %1 = sitofp i32 %x_0 to double
  ret double %1
}

define i32 @double_to_int(double %x_0) {
  %1 = fptosi double %x_0 to i32
  ret i32 %1
}

declare i32 @length(%IntList*)

define i32 @length.1(%IntList* %l_0) {
  %1 = ptrtoint %IntList* %l_0 to i32
  %2 = ptrtoint %IntList* null to i32
  %3 = icmp eq i32 %1, %2
  br i1 %3, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  br label %if.if_exit_0

if.else_0:                                        ; preds = %0
  %4 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 1
  %5 = load %IntList*, %IntList** %4, align 8
  %6 = call i32 @length.1(%IntList* %5)
  %7 = add i32 1, %6
  br label %if.else_exit_0

if.if_exit_0:                                     ; preds = %if.then_0
  br label %if.end_0

if.else_exit_0:                                   ; preds = %if.else_0
  br label %if.end_0

if.end_0:                                         ; preds = %if.else_exit_0, %if.if_exit_0
  %8 = phi i32 [ 0, %if.if_exit_0 ], [ %7, %if.else_exit_0 ]
  ret i32 %8
}

declare i1 @all(%IntList*, i1 (i32)*)

define i1 @all.2(%IntList* %l_0, i1 (i32)* %f_0) {
  %1 = ptrtoint %IntList* %l_0 to i32
  %2 = ptrtoint %IntList* null to i32
  %3 = icmp eq i32 %1, %2
  br i1 %3, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  br label %if.if_exit_1

if.else_0:                                        ; preds = %0
  %4 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 0
  %5 = load i32, i32* %4, align 4
  %6 = call i1 %f_0(i32 %5)
  br i1 %6, label %if.then_1, label %if.else_1

if.then_1:                                        ; preds = %if.else_0
  %7 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 1
  %8 = load %IntList*, %IntList** %7, align 8
  %9 = call i1 @all.2(%IntList* %8, i1 (i32)* %f_0)
  br label %if.if_exit_0

if.else_1:                                        ; preds = %if.else_0
  br label %if.else_exit_0

if.if_exit_0:                                     ; preds = %if.then_1
  br label %if.end_0

if.else_exit_0:                                   ; preds = %if.else_1
  br label %if.end_0

if.end_0:                                         ; preds = %if.else_exit_0, %if.if_exit_0
  %10 = phi i1 [ %9, %if.if_exit_0 ], [ false, %if.else_exit_0 ]
  br label %if.else_exit_1

if.if_exit_1:                                     ; preds = %if.then_0
  br label %if.end_1

if.else_exit_1:                                   ; preds = %if.end_0
  br label %if.end_1

if.end_1:                                         ; preds = %if.else_exit_1, %if.if_exit_1
  %11 = phi i1 [ true, %if.if_exit_1 ], [ %10, %if.else_exit_1 ]
  ret i1 %11
}

declare i1 @any(%IntList*, i1 (i32)*)

define i1 @any.3(%IntList* %l_0, i1 (i32)* %f_0) {
  %1 = ptrtoint %IntList* %l_0 to i32
  %2 = ptrtoint %IntList* null to i32
  %3 = icmp eq i32 %1, %2
  br i1 %3, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  br label %if.if_exit_1

if.else_0:                                        ; preds = %0
  %4 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 0
  %5 = load i32, i32* %4, align 4
  %6 = call i1 %f_0(i32 %5)
  br i1 %6, label %if.then_1, label %if.else_1

if.then_1:                                        ; preds = %if.else_0
  br label %if.if_exit_0

if.else_1:                                        ; preds = %if.else_0
  %7 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 1
  %8 = load %IntList*, %IntList** %7, align 8
  %9 = call i1 @any.3(%IntList* %8, i1 (i32)* %f_0)
  br label %if.else_exit_0

if.if_exit_0:                                     ; preds = %if.then_1
  br label %if.end_0

if.else_exit_0:                                   ; preds = %if.else_1
  br label %if.end_0

if.end_0:                                         ; preds = %if.else_exit_0, %if.if_exit_0
  %10 = phi i1 [ true, %if.if_exit_0 ], [ %9, %if.else_exit_0 ]
  br label %if.else_exit_1

if.if_exit_1:                                     ; preds = %if.then_0
  br label %if.end_1

if.else_exit_1:                                   ; preds = %if.end_0
  br label %if.end_1

if.end_1:                                         ; preds = %if.else_exit_1, %if.if_exit_1
  %11 = phi i1 [ false, %if.if_exit_1 ], [ %10, %if.else_exit_1 ]
  ret i1 %11
}

declare %IntList* @map_int(%IntList*, i32 (i32)*)

define %IntList* @map_int.4(%IntList* %l_0, i32 (i32)* %f_0) {
  %1 = ptrtoint %IntList* %l_0 to i32
  %2 = ptrtoint %IntList* null to i32
  %3 = icmp eq i32 %1, %2
  br i1 %3, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  br label %if.if_exit_0

if.else_0:                                        ; preds = %0
  %4 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 0
  %5 = load i32, i32* %4, align 4
  %6 = call i32 %f_0(i32 %5)
  %7 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 1
  %8 = load %IntList*, %IntList** %7, align 8
  %9 = call %IntList* @map_int.4(%IntList* %8, i32 (i32)* %f_0)
  %10 = call %IntList* @_alloc_int_list_node()
  %11 = getelementptr %IntList, %IntList* %10, i32 0, i32 0
  store i32 %6, i32* %11, align 4
  %12 = getelementptr %IntList, %IntList* %10, i32 0, i32 1
  store %IntList* %9, %IntList** %12, align 8
  br label %if.else_exit_0

if.if_exit_0:                                     ; preds = %if.then_0
  br label %if.end_0

if.else_exit_0:                                   ; preds = %if.else_0
  br label %if.end_0

if.end_0:                                         ; preds = %if.else_exit_0, %if.if_exit_0
  %13 = phi %IntList* [ null, %if.if_exit_0 ], [ %10, %if.else_exit_0 ]
  ret %IntList* %13
}

declare %IntList* @filter_int(%IntList*, i1 (i32)*)

define %IntList* @filter_int.5(%IntList* %l_0, i1 (i32)* %f_0) {
  %1 = ptrtoint %IntList* %l_0 to i32
  %2 = ptrtoint %IntList* null to i32
  %3 = icmp eq i32 %1, %2
  br i1 %3, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  br label %if.if_exit_1

if.else_0:                                        ; preds = %0
  %4 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 0
  %5 = load i32, i32* %4, align 4
  %6 = call i1 %f_0(i32 %5)
  br i1 %6, label %if.then_1, label %if.else_1

if.then_1:                                        ; preds = %if.else_0
  %7 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 0
  %8 = load i32, i32* %7, align 4
  %9 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 1
  %10 = load %IntList*, %IntList** %9, align 8
  %11 = call %IntList* @filter_int.5(%IntList* %10, i1 (i32)* %f_0)
  %12 = call %IntList* @_alloc_int_list_node()
  %13 = getelementptr %IntList, %IntList* %12, i32 0, i32 0
  store i32 %8, i32* %13, align 4
  %14 = getelementptr %IntList, %IntList* %12, i32 0, i32 1
  store %IntList* %11, %IntList** %14, align 8
  br label %if.if_exit_0

if.else_1:                                        ; preds = %if.else_0
  %15 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 1
  %16 = load %IntList*, %IntList** %15, align 8
  %17 = call %IntList* @filter_int.5(%IntList* %16, i1 (i32)* %f_0)
  br label %if.else_exit_0

if.if_exit_0:                                     ; preds = %if.then_1
  br label %if.end_0

if.else_exit_0:                                   ; preds = %if.else_1
  br label %if.end_0

if.end_0:                                         ; preds = %if.else_exit_0, %if.if_exit_0
  %18 = phi %IntList* [ %12, %if.if_exit_0 ], [ %17, %if.else_exit_0 ]
  br label %if.else_exit_1

if.if_exit_1:                                     ; preds = %if.then_0
  br label %if.end_1

if.else_exit_1:                                   ; preds = %if.end_0
  br label %if.end_1

if.end_1:                                         ; preds = %if.else_exit_1, %if.if_exit_1
  %19 = phi %IntList* [ null, %if.if_exit_1 ], [ %18, %if.else_exit_1 ]
  ret %IntList* %19
}

declare i32 @nth_int(%IntList*, i32)

define i32 @nth_int.6(%IntList* %l_0, i32 %n_0) {
  %1 = icmp eq i32 %n_0, 0
  br i1 %1, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  %2 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 0
  %3 = load i32, i32* %2, align 4
  br label %if.if_exit_0

if.else_0:                                        ; preds = %0
  %4 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 1
  %5 = load %IntList*, %IntList** %4, align 8
  %6 = sub i32 %n_0, 1
  %7 = call i32 @nth_int.6(%IntList* %5, i32 %6)
  br label %if.else_exit_0

if.if_exit_0:                                     ; preds = %if.then_0
  br label %if.end_0

if.else_exit_0:                                   ; preds = %if.else_0
  br label %if.end_0

if.end_0:                                         ; preds = %if.else_exit_0, %if.if_exit_0
  %8 = phi i32 [ %3, %if.if_exit_0 ], [ %7, %if.else_exit_0 ]
  ret i32 %8
}

declare %IntList* @reverse_acc_int(%IntList*, %IntList*)

define %IntList* @reverse_acc_int.7(%IntList* %l_0, %IntList* %acc_0) {
  %1 = ptrtoint %IntList* %l_0 to i32
  %2 = ptrtoint %IntList* null to i32
  %3 = icmp eq i32 %1, %2
  br i1 %3, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  br label %if.if_exit_0

if.else_0:                                        ; preds = %0
  %4 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 1
  %5 = load %IntList*, %IntList** %4, align 8
  %6 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 0
  %7 = load i32, i32* %6, align 4
  %8 = call %IntList* @_alloc_int_list_node()
  %9 = getelementptr %IntList, %IntList* %8, i32 0, i32 0
  store i32 %7, i32* %9, align 4
  %10 = getelementptr %IntList, %IntList* %8, i32 0, i32 1
  store %IntList* %acc_0, %IntList** %10, align 8
  %11 = call %IntList* @reverse_acc_int.7(%IntList* %5, %IntList* %8)
  br label %if.else_exit_0

if.if_exit_0:                                     ; preds = %if.then_0
  br label %if.end_0

if.else_exit_0:                                   ; preds = %if.else_0
  br label %if.end_0

if.end_0:                                         ; preds = %if.else_exit_0, %if.if_exit_0
  %12 = phi %IntList* [ %acc_0, %if.if_exit_0 ], [ %11, %if.else_exit_0 ]
  ret %IntList* %12
}

declare %IntList* @reverse_int(%IntList*)

define %IntList* @reverse_int.8(%IntList* %l_0) {
  %1 = call %IntList* @reverse_acc_int.7(%IntList* %l_0, %IntList* null)
  ret %IntList* %1
}

declare i32 @foldl_int(%IntList*, i32, i32 (i32, i32)*)

define i32 @foldl_int.9(%IntList* %l_0, i32 %acc_0, i32 (i32, i32)* %f_0) {
  %1 = ptrtoint %IntList* %l_0 to i32
  %2 = ptrtoint %IntList* null to i32
  %3 = icmp eq i32 %1, %2
  br i1 %3, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  br label %if.if_exit_0

if.else_0:                                        ; preds = %0
  %4 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 1
  %5 = load %IntList*, %IntList** %4, align 8
  %6 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 0
  %7 = load i32, i32* %6, align 4
  %8 = call i32 %f_0(i32 %acc_0, i32 %7)
  %9 = call i32 @foldl_int.9(%IntList* %5, i32 %8, i32 (i32, i32)* %f_0)
  br label %if.else_exit_0

if.if_exit_0:                                     ; preds = %if.then_0
  br label %if.end_0

if.else_exit_0:                                   ; preds = %if.else_0
  br label %if.end_0

if.end_0:                                         ; preds = %if.else_exit_0, %if.if_exit_0
  %10 = phi i32 [ %acc_0, %if.if_exit_0 ], [ %9, %if.else_exit_0 ]
  ret i32 %10
}

declare i32 @foldr_int(%IntList*, i32, i32 (i32, i32)*)

define i32 @foldr_int.10(%IntList* %l_0, i32 %acc_0, i32 (i32, i32)* %f_0) {
  %1 = ptrtoint %IntList* %l_0 to i32
  %2 = ptrtoint %IntList* null to i32
  %3 = icmp eq i32 %1, %2
  br i1 %3, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  br label %if.if_exit_0

if.else_0:                                        ; preds = %0
  %4 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 0
  %5 = load i32, i32* %4, align 4
  %6 = getelementptr %IntList, %IntList* %l_0, i32 0, i32 1
  %7 = load %IntList*, %IntList** %6, align 8
  %8 = call i32 @foldr_int.10(%IntList* %7, i32 %acc_0, i32 (i32, i32)* %f_0)
  %9 = call i32 %f_0(i32 %5, i32 %8)
  br label %if.else_exit_0

if.if_exit_0:                                     ; preds = %if.then_0
  br label %if.end_0

if.else_exit_0:                                   ; preds = %if.else_0
  br label %if.end_0

if.end_0:                                         ; preds = %if.else_exit_0, %if.if_exit_0
  %10 = phi i32 [ %acc_0, %if.if_exit_0 ], [ %9, %if.else_exit_0 ]
  ret i32 %10
}

declare i32 @min_index_int(%IntList*, i32)

define i32 @min_index_int.11(%IntList* %arr_0, i32 %start_0) {
  %1 = call i32 @length.1(%IntList* %arr_0)
  %2 = sub i32 %1, 1
  %3 = icmp sge i32 %start_0, %2
  br i1 %3, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  br label %if.if_exit_1

if.else_0:                                        ; preds = %0
  %4 = call i32 @nth_int.6(%IntList* %arr_0, i32 %start_0)
  %5 = add i32 %start_0, 1
  %6 = call i32 @min_index_int.11(%IntList* %arr_0, i32 %5)
  %7 = call i32 @nth_int.6(%IntList* %arr_0, i32 %6)
  %8 = icmp slt i32 %4, %7
  br i1 %8, label %if.then_1, label %if.else_1

if.then_1:                                        ; preds = %if.else_0
  br label %if.if_exit_0

if.else_1:                                        ; preds = %if.else_0
  %9 = add i32 %start_0, 1
  %10 = call i32 @min_index_int.11(%IntList* %arr_0, i32 %9)
  br label %if.else_exit_0

if.if_exit_0:                                     ; preds = %if.then_1
  br label %if.end_0

if.else_exit_0:                                   ; preds = %if.else_1
  br label %if.end_0

if.end_0:                                         ; preds = %if.else_exit_0, %if.if_exit_0
  %11 = phi i32 [ %start_0, %if.if_exit_0 ], [ %10, %if.else_exit_0 ]
  br label %if.else_exit_1

if.if_exit_1:                                     ; preds = %if.then_0
  br label %if.end_1

if.else_exit_1:                                   ; preds = %if.end_0
  br label %if.end_1

if.end_1:                                         ; preds = %if.else_exit_1, %if.if_exit_1
  %12 = phi i32 [ %start_0, %if.if_exit_1 ], [ %11, %if.else_exit_1 ]
  ret i32 %12
}

declare { i32, i32 }* @_internal_find_min_int(%IntList*, i32, i32, i32)

define { i32, i32 }* @_internal_find_min_int.12(%IntList* %arr_0, i32 %min_val_0, i32 %min_idx_0, i32 %acc_0) {
  %1 = ptrtoint %IntList* %arr_0 to i32
  %2 = ptrtoint %IntList* null to i32
  %3 = icmp eq i32 %1, %2
  br i1 %3, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  %4 = alloca { i32, i32 }, align 8
  %5 = getelementptr { i32, i32 }, { i32, i32 }* %4, i32 0, i32 0
  store volatile i32 %min_val_0, i32* %5, align 4
  %6 = getelementptr { i32, i32 }, { i32, i32 }* %4, i32 0, i32 1
  store volatile i32 %min_idx_0, i32* %6, align 4
  br label %if.if_exit_1

if.else_0:                                        ; preds = %0
  %7 = getelementptr %IntList, %IntList* %arr_0, i32 0, i32 0
  %8 = load i32, i32* %7, align 4
  %9 = icmp slt i32 %8, %min_val_0
  br i1 %9, label %if.then_1, label %if.else_1

if.then_1:                                        ; preds = %if.else_0
  %10 = getelementptr %IntList, %IntList* %arr_0, i32 0, i32 1
  %11 = load %IntList*, %IntList** %10, align 8
  %12 = add i32 %acc_0, 1
  %13 = call { i32, i32 }* @_internal_find_min_int.12(%IntList* %11, i32 %8, i32 %acc_0, i32 %12)
  br label %if.if_exit_0

if.else_1:                                        ; preds = %if.else_0
  %14 = getelementptr %IntList, %IntList* %arr_0, i32 0, i32 1
  %15 = load %IntList*, %IntList** %14, align 8
  %16 = add i32 %acc_0, 1
  %17 = call { i32, i32 }* @_internal_find_min_int.12(%IntList* %15, i32 %min_val_0, i32 %min_idx_0, i32 %16)
  br label %if.else_exit_0

if.if_exit_0:                                     ; preds = %if.then_1
  br label %if.end_0

if.else_exit_0:                                   ; preds = %if.else_1
  br label %if.end_0

if.end_0:                                         ; preds = %if.else_exit_0, %if.if_exit_0
  %18 = phi { i32, i32 }* [ %13, %if.if_exit_0 ], [ %17, %if.else_exit_0 ]
  br label %if.else_exit_1

if.if_exit_1:                                     ; preds = %if.then_0
  br label %if.end_1

if.else_exit_1:                                   ; preds = %if.end_0
  br label %if.end_1

if.end_1:                                         ; preds = %if.else_exit_1, %if.if_exit_1
  %19 = phi { i32, i32 }* [ %4, %if.if_exit_1 ], [ %18, %if.else_exit_1 ]
  ret { i32, i32 }* %19
}

declare { i32, i32 }* @find_min_int(%IntList*, i32)

define { i32, i32 }* @find_min_int.13(%IntList* %arr_0, i32 %min_val_0) {
  %1 = call { i32, i32 }* @_internal_find_min_int.12(%IntList* %arr_0, i32 %min_val_0, i32 0, i32 0)
  ret { i32, i32 }* %1
}

declare %IntList* @remove_nth_int(%IntList*, i32)

define %IntList* @remove_nth_int.14(%IntList* %arr_0, i32 %n_0) {
  %1 = icmp eq i32 %n_0, 0
  br i1 %1, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  %2 = getelementptr %IntList, %IntList* %arr_0, i32 0, i32 1
  %3 = load %IntList*, %IntList** %2, align 8
  br label %if.if_exit_0

if.else_0:                                        ; preds = %0
  %4 = getelementptr %IntList, %IntList* %arr_0, i32 0, i32 0
  %5 = load i32, i32* %4, align 4
  %6 = getelementptr %IntList, %IntList* %arr_0, i32 0, i32 1
  %7 = load %IntList*, %IntList** %6, align 8
  %8 = sub i32 %n_0, 1
  %9 = call %IntList* @remove_nth_int.14(%IntList* %7, i32 %8)
  %10 = call %IntList* @_alloc_int_list_node()
  %11 = getelementptr %IntList, %IntList* %10, i32 0, i32 0
  store i32 %5, i32* %11, align 4
  %12 = getelementptr %IntList, %IntList* %10, i32 0, i32 1
  store %IntList* %9, %IntList** %12, align 8
  br label %if.else_exit_0

if.if_exit_0:                                     ; preds = %if.then_0
  br label %if.end_0

if.else_exit_0:                                   ; preds = %if.else_0
  br label %if.end_0

if.end_0:                                         ; preds = %if.else_exit_0, %if.if_exit_0
  %13 = phi %IntList* [ %3, %if.if_exit_0 ], [ %10, %if.else_exit_0 ]
  ret %IntList* %13
}

declare %IntList* @selection_sort_int(%IntList*)

define %IntList* @selection_sort_int.15(%IntList* %arr_0) {
  %1 = ptrtoint %IntList* %arr_0 to i32
  %2 = ptrtoint %IntList* null to i32
  %3 = icmp eq i32 %1, %2
  br i1 %3, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  br label %if.if_exit_0

if.else_0:                                        ; preds = %0
  %4 = call { i32, i32 }* @find_min_int.13(%IntList* %arr_0, i32 9999999)
  %5 = load { i32, i32 }, { i32, i32 }* %4, align 4
  %6 = extractvalue { i32, i32 } %5, 0
  %7 = load { i32, i32 }, { i32, i32 }* %4, align 4
  %8 = extractvalue { i32, i32 } %7, 1
  %9 = call %IntList* @remove_nth_int.14(%IntList* %arr_0, i32 %8)
  %10 = call %IntList* @selection_sort_int.15(%IntList* %9)
  %11 = call %IntList* @_alloc_int_list_node()
  %12 = getelementptr %IntList, %IntList* %11, i32 0, i32 0
  store i32 %6, i32* %12, align 4
  %13 = getelementptr %IntList, %IntList* %11, i32 0, i32 1
  store %IntList* %10, %IntList** %13, align 8
  br label %if.else_exit_0

if.if_exit_0:                                     ; preds = %if.then_0
  br label %if.end_0

if.else_exit_0:                                   ; preds = %if.else_0
  br label %if.end_0

if.end_0:                                         ; preds = %if.else_exit_0, %if.if_exit_0
  %14 = phi %IntList* [ null, %if.if_exit_0 ], [ %11, %if.else_exit_0 ]
  ret %IntList* %14
}

declare %IntList* @rand_array(i32)

define %IntList* @rand_array.16(i32 %n_0) {
  %1 = icmp eq i32 %n_0, 0
  br i1 %1, label %if.then_0, label %if.else_0

if.then_0:                                        ; preds = %0
  br label %if.if_exit_0

if.else_0:                                        ; preds = %0
  %2 = call i32 @rand()
  %3 = udiv i32 %2, 100000
  %4 = sub i32 %n_0, 1
  %5 = call %IntList* @rand_array.16(i32 %4)
  %6 = call %IntList* @_alloc_int_list_node()
  %7 = getelementptr %IntList, %IntList* %6, i32 0, i32 0
  store i32 %3, i32* %7, align 4
  %8 = getelementptr %IntList, %IntList* %6, i32 0, i32 1
  store %IntList* %5, %IntList** %8, align 8
  br label %if.else_exit_0

if.if_exit_0:                                     ; preds = %if.then_0
  br label %if.end_0

if.else_exit_0:                                   ; preds = %if.else_0
  br label %if.end_0

if.end_0:                                         ; preds = %if.else_exit_0, %if.if_exit_0
  %9 = phi %IntList* [ null, %if.if_exit_0 ], [ %6, %if.else_exit_0 ]
  ret %IntList* %9
}

define i32 @main() {
  %1 = call %IntList* @rand_array.16(i32 5000)
  %2 = call %IntList* @selection_sort_int.15(%IntList* %1)
  %3 = call i32 @printil(%IntList* %2)
  ret i32 0
}

attributes #0 = { optnone }
