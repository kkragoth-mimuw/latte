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
	%r2 = add i32 1, 1
	%r3 = add i32 %r2, 1
	store i32 %r3, i32* %r1
	%r4 = load i32, i32* %r1
	%r5 = icmp sle i32 %r4, 2
	br i1 %r5, label %L1, label %L2
L1:
	%r6 = load i32, i32* %r1
	%r7 = icmp sle i32 %r6, 1
	br i1 %r7, label %L4, label %L5
L2:
	store i32 4, i32* %r0
	br label %L3
L3:
	%r10 = load i32, i32* %r0
	call void @printInt(i32 %r10)
	ret i32 0
L4:
	%r8 = load i32, i32* %r1
	%r9 = icmp sle i32 %r8, 0
	br i1 %r9, label %L6, label %L7
L5:
	store i32 3, i32* %r0
	br label %L3
L6:
	store i32 1, i32* %r0
	br label %L3
L7:
	store i32 2, i32* %r0
	br label %L3
}

