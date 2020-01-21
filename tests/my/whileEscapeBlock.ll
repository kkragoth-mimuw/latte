declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)








define i32 @main() {
L0:
	%r0 = alloca i32
	store i32 0, i32* %r0
	%r1 = alloca i32
	store i32 0, i32* %r1
	%r2 = alloca i32
	store i32 0, i32* %r2
	%r3 = alloca i32
	store i32 0, i32* %r3
	%r4 = alloca i32
	store i32 0, i32* %r4
	%r5 = alloca i32
	store i32 0, i32* %r5
	store i32 0, i32* %r0
	store i32 0, i32* %r1
	store i32 0, i32* %r2
	store i32 1, i32* %r3
	%r6 = load i32, i32* %r0
	%r7 = load i32, i32* %r3
	%r8 = icmp slt i32 %r6, %r7
	br i1 %r8, label %L1, label %L2
L1:
	br label %L3
L2:
	%r19 = load i32, i32* %r0
	call void @printInt(i32 %r19)
	ret i32 0
L3:
	%r9 = load i32, i32* %r1
	%r10 = icmp slt i32 %r9, 5
	br i1 %r10, label %L4, label %L5
L4:
	%r11 = load i32, i32* %r1
	%r12 = add i32 %r11, 1
	store i32 %r12, i32* %r1
	br label %L6
L5:
	store i32 2, i32* %r5
	br label %L2
L6:
	%r13 = load i32, i32* %r2
	%r14 = icmp slt i32 %r13, 5
	br i1 %r14, label %L7, label %L8
L7:
	%r15 = load i32, i32* %r2
	%r16 = add i32 %r15, 1
	store i32 %r16, i32* %r2
	%r17 = load i32, i32* %r0
	%r18 = add i32 %r17, 2
	store i32 %r18, i32* %r0
	br label %L6
L8:
	store i32 2, i32* %r4
	br label %L3
}

