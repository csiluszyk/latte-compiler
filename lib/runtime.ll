@dnl = internal constant [4 x i8] c"%d\0A\00"
@d   = internal constant [3 x i8] c"%d\00"
@.str = private constant [15 x i8] c"runtime error\0A\00"

declare void @exit(i32)
declare i32 @printf(i8*, ...)
declare i32 @scanf(i8*, ...)
declare i32 @puts(i8*)
declare noalias i8* @malloc(i32) nounwind
declare i32 @strlen(i8* nocapture) nounwind readonly
declare i8* @strcat(i8*, i8* nocapture) nounwind
declare i8* @strcpy(i8*, i8* nocapture) nounwind
declare i32 @getchar()
declare i8* @realloc(i8*, i64)
declare i32 @strcmp(i8*, i8*)

define void @error() {
entry:
    %s = getelementptr [15 x i8], [15 x i8]* @.str, i32 0, i32 0
    call i32 @puts(i8* %s)
    call void @exit(i32 1)
    ret void
}

define void @printInt(i32 %x) {
entry:
    %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
    call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
    ret void
}

define void @printString(i8* %s) {
entry:
    call i32 @puts(i8* %s)
    ret void
}

define i32 @readInt() {
entry:
    %res = alloca i32
    %t1 = getelementptr [3 x i8], [3 x i8]* @d, i32 0, i32 0
    call i32 (i8*, ...) @scanf(i8* %t1, i32* %res)
    %t2 = load i32, i32* %res
    ret i32 %t2
}

define i8* @_concat(i8* %s1, i8* %s2) {
    %1 = call i32 @strlen(i8* %s1)
    %2 = call i32 @strlen(i8* %s2)
    %3 = add i32 %1, 1
    %4 = add i32 %3, %2
    %5 = call i8* @malloc(i32 %4)
    %6 = call i8* @strcpy(i8* %5, i8* %s1)
    %7 = call i8* @strcat(i8* %6, i8* %s2)
    ret i8* %7
}

define i1 @_streq(i8*, i8*) {
    %3 = tail call i32 @strcmp(i8* %0, i8* %1)
    %4 = icmp eq i32 %3, 0
    ret i1 %4
}

define i8* @readString() {
    %1 = alloca i32, align 4
    %2 = alloca i8*, align 8
    %3 = alloca i32, align 4
    %4 = alloca i64, align 8
    store i32 127, i32* %1, align 4
    store i64 0, i64* %4, align 8
    %5 = load i32, i32* %1, align 4
    %6 = sext i32 %5 to i64
    %7 = mul i64 1, %6
    %8 = call i8* @realloc(i8* null, i64 %7)
    store i8* %8, i8** %2, align 8
    %9 = load i8*, i8** %2, align 8
    %10 = icmp ne i8* %9, null
    br i1 %10, label %12, label %11

; <label>:11:
    call void @error()
    br label %12

; <label>:12:
    br label %13

; <label>:13:
    %14 = call i32 @getchar()
    store i32 %14, i32* %3, align 4
    %15 = icmp ne i32 -1, %14
    br i1 %15, label %16, label %19

; <label>:16:
    %17 = load i32, i32* %3, align 4
    %18 = icmp ne i32 %17, 10
    br label %19

; <label>:19:
    %20 = phi i1 [ false, %13 ], [ %18, %16 ]
    br i1 %20, label %21, label %44

; <label>:21:
    %22 = load i32, i32* %3, align 4
    %23 = trunc i32 %22 to i8
    %24 = load i8*, i8** %2, align 8
    %25 = load i64, i64* %4, align 8
    %26 = add i64 %25, 1
    store i64 %26, i64* %4, align 8
    %27 = getelementptr inbounds i8, i8* %24, i64 %25
    store i8 %23, i8* %27, align 1
    %28 = load i64, i64* %4, align 8
    %29 = load i32, i32* %1, align 4
    %30 = sext i32 %29 to i64
    %31 = icmp eq i64 %28, %30
    br i1 %31, label %32, label %43

; <label>:32:
    %33 = load i8*, i8** %2, align 8
    %34 = load i32, i32* %1, align 4
    %35 = mul nsw i32 %34, 2
    store i32 %35, i32* %1, align 4
    %36 = sext i32 %35 to i64
    %37 = mul i64 1, %36
    %38 = call i8* @realloc(i8* %33, i64 %37)
    store i8* %38, i8** %2, align 8
    %39 = load i8*, i8** %2, align 8
    %40 = icmp ne i8* %39, null
    br i1 %40, label %42, label %41

; <label>:41:
    call void @error()
    br label %42

; <label>:42:
    br label %43

; <label>:43:
    br label %13

; <label>:44:
    %45 = load i8*, i8** %2, align 8
    %46 = load i64, i64* %4, align 8
    %47 = add i64 %46, 1
    store i64 %47, i64* %4, align 8
    %48 = getelementptr inbounds i8, i8* %45, i64 %46
    store i8 0, i8* %48, align 1
    %49 = load i8*, i8** %2, align 8
    %50 = load i64, i64* %4, align 8
    %51 = mul i64 1, %50
    %52 = call i8* @realloc(i8* %49, i64 %51)
    store i8* %52, i8** %2, align 8
    %53 = load i8*, i8** %2, align 8
    %54 = icmp ne i8* %53, null
    br i1 %54, label %56, label %55

; <label>:55:
    call void @error()
    br label %56

; <label>:56:
    %57 = load i8*, i8** %2, align 8
    ret i8* %57
}
