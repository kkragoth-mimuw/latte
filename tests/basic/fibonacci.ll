declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s1 = private constant [42 x i8] c"Expected a non-negative integer, but got:\00"






define i32 @fibonacci(i32 %n) {
L0:
	%r0 = alloca i32
	store i32 %n, i32* %r0
	%r1 = alloca i32
	store i32 0, i32* %r1
	%r2 = alloca i32
	store i32 0, i32* %r2
	%r3 = alloca i32
	store i32 0, i32* %r3
	%r4 = alloca i32
	store i32 0, i32* %r4
	%r5 = load i32, i32* %r0
	%r6 = icmp sle i32 %r5, 1
	br i1 %r6, label %L1, label %L2
L1:
	%r7 = load i32, i32* %r0
	ret i32 %r7
L2:
	store i32 0, i32* %r1
	store i32 1, i32* %r2
	store i32 2, i32* %r4
	br label %L3
L3:
	%r8 = load i32, i32* %r4
	%r9 = load i32, i32* %r0
	%r10 = icmp sle i32 %r8, %r9
	br i1 %r10, label %L4, label %L5
L4:
	%r11 = load i32, i32* %r2
	%r12 = load i32, i32* %r1
	%r13 = add i32 %r11, %r12
	store i32 %r13, i32* %r3
	%r14 = load i32, i32* %r2
	store i32 %r14, i32* %r1
	%r15 = load i32, i32* %r3
	store i32 %r15, i32* %r2
	%r16 = load i32, i32* %r4
	%r17 = add i32 %r16, 1
	store i32 %r17, i32* %r4
	br label %L3
L5:
	%r18 = load i32, i32* %r2
	ret i32 %r18
}

define i32 @main() {
L0:
	%r0 = alloca i32
	store i32 0, i32* %r0
	%r1 = call i32 @readInt()
	store i32 %r1, i32* %r0
	%r2 = load i32, i32* %r0
	%r3 = icmp sge i32 %r2, 0
	br i1 %r3, label %L1, label %L2
L1:
	%r4 = load i32, i32* %r0
	%r5 = call i32 @fibonacci(i32 %r4)
	call void @printInt(i32 %r5)
	ret i32 0
L2:
	%r6 = bitcast [42 x i8]* @s1 to i8*
	call void @printString(i8* %r6)
	%r7 = load i32, i32* %r0
	call void @printInt(i32 %r7)
	ret i32 1
}

