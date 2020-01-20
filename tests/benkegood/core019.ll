declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)

@s1 = private constant [4 x i8] c"foo\00"






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
	store i32 78, i32* %r0
	store i32 1, i32* %r1
	%r4 = load i32, i32* %r1
	call void @printInt(i32 %r4)
	%r5 = load i32, i32* %r0
	call void @printInt(i32 %r5)
	br label %L1
L1:
	%r6 = load i32, i32* %r0
	%r7 = icmp sgt i32 %r6, 76
	br i1 %r7, label %L2, label %L3
L2:
	%r8 = load i32, i32* %r0
	%r9 = sub i32 %r8, 1
	store i32 %r9, i32* %r0
	%r10 = load i32, i32* %r0
	call void @printInt(i32 %r10)
	%r11 = load i32, i32* %r0
	%r12 = add i32 %r11, 7
	store i32 %r12, i32* %r2
	%r13 = load i32, i32* %r2
	call void @printInt(i32 %r13)
	br label %L1
L3:
	%r14 = load i32, i32* %r0
	call void @printInt(i32 %r14)
	%r15 = load i32, i32* %r0
	%r16 = icmp sgt i32 %r15, 4
	br i1 %r16, label %L4, label %L5
L4:
	store i32 4, i32* %r3
	%r17 = load i32, i32* %r3
	call void @printInt(i32 %r17)
	br label %L6
L5:
	%r18 = bitcast [4 x i8]* @s1 to i8*
	call void @printString(i8* %r18)
	br label %L6
L6:
	%r19 = load i32, i32* %r0
	call void @printInt(i32 %r19)
	ret i32 0
}

