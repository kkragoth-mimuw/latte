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
	%r6 = alloca i32
	store i32 0, i32* %r6
	store i32 0, i32* %r0
	store i32 0, i32* %r1
	store i32 0, i32* %r2
	store i32 1, i32* %r3
	store i32 0, i32* %r4
	%r7 = load i32, i32* %r0
	%r8 = load i32, i32* %r3
	%r9 = icmp slt i32 %r7, %r8
	br i1 %r9, label %L1, label %L2
L1:
	store i32 5, i32* %r4
	br label %L3
L2:
	%r20 = load i32, i32* %r0
	call void @printInt(i32 %r20)
	ret i32 0
L3:
	%r10 = load i32, i32* %r1
	%r11 = icmp slt i32 %r10, 5
	br i1 %r11, label %L4, label %L5
L4:
	%r12 = load i32, i32* %r1
	%r13 = add i32 %r12, 1
	store i32 %r13, i32* %r1
	br label %L6
L5:
	store i32 2, i32* %r6
	br label %L2
L6:
	%r14 = load i32, i32* %r2
	%r15 = icmp slt i32 %r14, 5
	br i1 %r15, label %L7, label %L8
L7:
	%r16 = load i32, i32* %r2
	%r17 = add i32 %r16, 1
	store i32 %r17, i32* %r2
	%r18 = load i32, i32* %r0
	%r19 = add i32 %r18, 2
	store i32 %r19, i32* %r0
	br label %L6
L8:
	store i32 2, i32* %r5
	br label %L3
}

