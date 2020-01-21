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
	store i32 0, i32* %r0
	store i32 0, i32* %r1
	store i32 0, i32* %r2
	store i32 1, i32* %r3
	%r4 = load i32, i32* %r0
	%r5 = load i32, i32* %r3
	%r6 = icmp slt i32 %r4, %r5
	br i1 %r6, label %L1, label %L2
L1:
	br label %L3
L2:
	%r15 = load i32, i32* %r0
	call void @printInt(i32 %r15)
	ret i32 0
L3:
	%r7 = load i32, i32* %r1
	%r8 = icmp slt i32 %r7, 5
	br i1 %r8, label %L4, label %L2
L4:
	%r9 = load i32, i32* %r1
	%r10 = add i32 %r9, 1
	store i32 %r10, i32* %r1
	br label %L6
L6:
	%r11 = load i32, i32* %r2
	%r12 = icmp slt i32 %r11, 10
	br i1 %r12, label %L7, label %L2
L7:
	%r13 = load i32, i32* %r2
	%r14 = add i32 %r13, 1
	store i32 %r14, i32* %r2
	br label %L6
}

