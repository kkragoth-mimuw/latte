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
	%r3 = load i32, i32* %r0
	%r4 = load i32, i32* %r1
	%r5 = icmp eq i32 %r3, %r4
	br i1 %r5, label %L1, label %L2
L1:
	br label %L4
L2:
	br label %L7
L3:
	ret i32 0
L4:
	%r6 = load i32, i32* %r1
	%r7 = load i32, i32* %r2
	%r8 = icmp slt i32 %r6, %r7
	br i1 %r8, label %L5, label %L3
L5:
	%r9 = load i32, i32* %r2
	%r10 = add i32 %r9, 1
	store i32 %r10, i32* %r2
	br label %L4
L7:
	%r11 = load i32, i32* %r0
	%r12 = load i32, i32* %r1
	%r13 = icmp slt i32 %r11, %r12
	br i1 %r13, label %L8, label %L3
L8:
	%r14 = load i32, i32* %r2
	%r15 = add i32 %r14, 1
	store i32 %r15, i32* %r2
	br label %L7
}

