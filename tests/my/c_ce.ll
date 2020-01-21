declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)

@s2 = private constant [7 x i8] c"not ok\00"
@s1 = private constant [3 x i8] c"ok\00"






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
	%r5 = load i32, i32* %r0
	%r6 = load i32, i32* %r1
	%r7 = icmp eq i32 %r5, %r6
	br i1 %r7, label %L1, label %L2
L1:
	%r8 = load i32, i32* %r0
	%r9 = load i32, i32* %r1
	%r10 = icmp eq i32 %r8, %r9
	br i1 %r10, label %L3, label %L4
L2:
	ret i32 0
L3:
	%r11 = load i32, i32* %r2
	%r12 = load i32, i32* %r3
	%r13 = icmp eq i32 %r11, %r12
	br i1 %r13, label %L6, label %L7
L4:
	%r16 = load i32, i32* %r2
	%r17 = load i32, i32* %r3
	%r18 = icmp eq i32 %r16, %r17
	br i1 %r18, label %L8, label %L9
L5:
	store i32 2, i32* %r4
	br label %L2
L6:
	%r14 = bitcast [3 x i8]* @s1 to i8*
	call void @printString(i8* %r14)
	br label %L5
L7:
	%r15 = bitcast [7 x i8]* @s2 to i8*
	call void @printString(i8* %r15)
	br label %L5
L8:
	%r19 = bitcast [3 x i8]* @s1 to i8*
	call void @printString(i8* %r19)
	br label %L5
L9:
	%r20 = bitcast [7 x i8]* @s2 to i8*
	call void @printString(i8* %r20)
	br label %L5
}

