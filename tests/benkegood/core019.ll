declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s1 = private constant [4 x i8] c"foo\00"




define i32 @main() {
L0:
	%r0 = alloca i32
	store i32 78, i32* %r0
	%r1 = alloca i32
	store i32 1, i32* %r1
	%r2 = load i32, i32* %r1
	call void @printInt(i32 %r2)
	%r3 = load i32, i32* %r0
	call void @printInt(i32 %r3)
	br label %L1
L1:
	%r4 = load i32, i32* %r0
	%r5 = icmp sgt i32 %r4, 76
	br i1 %r5, label %L2, label %L3
L2:
	%r6 = load i32, i32* %r0
	%r7 = sub i32 %r6, 1
	store i32 %r7, i32* %r0
	%r8 = load i32, i32* %r0
	call void @printInt(i32 %r8)
	%r9 = load i32, i32* %r0
	%r10 = add i32 %r9, 7
	%r11 = alloca i32
	store i32 %r10, i32* %r11
	%r12 = load i32, i32* %r11
	call void @printInt(i32 %r12)
	br label %L1
L3:
	%r13 = load i32, i32* %r0
	call void @printInt(i32 %r13)
	%r14 = load i32, i32* %r0
	%r15 = icmp sgt i32 %r14, 4
	br i1 %r15, label %L4, label %L5
L4:
	%r16 = alloca i32
	store i32 4, i32* %r16
	%r17 = load i32, i32* %r16
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

