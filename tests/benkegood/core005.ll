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
	store i32 0, i32* %r1
	store i32 56, i32* %r1
	%r2 = load i32, i32* %r1
	%r3 = add i32 %r2, 45
	%r4 = icmp sle i32 %r3, 2
	br i1 %r4, label %L1, label %L2
L1:
	store i32 1, i32* %r0
	br label %L3
L2:
	store i32 2, i32* %r0
	br label %L3
L3:
	%r5 = load i32, i32* %r0
	call void @printInt(i32 %r5)
	ret i32 0
}

