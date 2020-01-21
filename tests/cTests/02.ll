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
	store i32 0, i32* %r0
	br label %L1
L1:
	%r1 = load i32, i32* %r0
	%r2 = icmp slt i32 %r1, 10
	br i1 %r2, label %L2, label %L3
L2:
	br label %L4
L3:
	%r13 = load i32, i32* %r0
	call void @printInt(i32 %r13)
	ret i32 0
L4:
	%r3 = load i32, i32* %r0
	%r4 = icmp slt i32 %r3, 10
	br i1 %r4, label %L5, label %L1
L5:
	br label %L7
L7:
	%r5 = load i32, i32* %r0
	%r6 = icmp slt i32 %r5, 10
	br i1 %r6, label %L8, label %L1
L8:
	br label %L10
L10:
	%r7 = load i32, i32* %r0
	%r8 = icmp slt i32 %r7, 10
	br i1 %r8, label %L11, label %L1
L11:
	br label %L13
L13:
	%r9 = load i32, i32* %r0
	%r10 = icmp slt i32 %r9, 10
	br i1 %r10, label %L14, label %L1
L14:
	%r11 = load i32, i32* %r0
	%r12 = add i32 %r11, 1
	store i32 %r12, i32* %r0
	br label %L13
}

