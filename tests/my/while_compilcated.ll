declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)






define i32 @main() {
L0:
	%r0 = alloca i32
	store i32 0, i32* %r0
	%r1 = alloca i32
	store i32 1, i32* %r1
	%r2 = alloca i32
	store i32 2, i32* %r2
	br label %L1
L1:
	%r3 = load i32, i32* %r0
	%r4 = load i32, i32* %r1
	%r5 = icmp slt i32 %r3, %r4
	br i1 %r5, label %L2, label %L3
L2:
	%r6 = load i32, i32* %r1
	%r7 = load i32, i32* %r2
	%r8 = icmp slt i32 %r6, %r7
	br label %L3
L3:
	%r9 = phi i1 [ %r5, %L1 ], [ %r8, %L2 ]
	br i1 %r9, label %L4, label %L5
L4:
	%r10 = load i32, i32* %r0
	%r11 = add i32 %r10, 1
	store i32 %r11, i32* %r0
	%r12 = load i32, i32* %r1
	%r13 = icmp ne i32 %r12, 5
	br i1 %r13, label %L6, label %L7
L5:
	ret i32 0
L6:
	%r14 = load i32, i32* %r1
	%r15 = add i32 %r14, 1
	store i32 %r15, i32* %r1
	br label %L7
L7:
	%r16 = load i32, i32* %r2
	%r17 = add i32 %r16, 1
	store i32 %r17, i32* %r2
	%r18 = load i32, i32* %r0
	call void @printInt(i32 %r18)
	br label %L1
}

