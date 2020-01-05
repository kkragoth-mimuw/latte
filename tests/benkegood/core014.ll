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
	%r2 = alloca i32
	store i32 0, i32* %r2
	store i32 1, i32* %r0
	%r3 = load i32, i32* %r0
	store i32 %r3, i32* %r1
	store i32 5000000, i32* %r2
	%r4 = load i32, i32* %r0
	call void @printInt(i32 %r4)
	br label %L1
L1:
	%r5 = load i32, i32* %r1
	%r6 = load i32, i32* %r2
	%r7 = icmp slt i32 %r5, %r6
	br i1 %r7, label %L2, label %L3
L2:
	%r8 = load i32, i32* %r1
	call void @printInt(i32 %r8)
	%r9 = load i32, i32* %r0
	%r10 = load i32, i32* %r1
	%r11 = add i32 %r9, %r10
	store i32 %r11, i32* %r1
	%r12 = load i32, i32* %r1
	%r13 = load i32, i32* %r0
	%r14 = sub i32 %r12, %r13
	store i32 %r14, i32* %r0
	br label %L1
L3:
	ret i32 0
}

