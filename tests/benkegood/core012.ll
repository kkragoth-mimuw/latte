declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s2 = private constant [2 x i8] c" \00"
@s3 = private constant [14 x i8] c"concatenation\00"
@s5 = private constant [6 x i8] c"false\00"
@s1 = private constant [7 x i8] c"string\00"
@s4 = private constant [5 x i8] c"true\00"






define i32 @main() {
L0:
	%r0 = alloca i32
	store i32 0, i32* %r0
	%r1 = alloca i32
	store i32 0, i32* %r1
	store i32 56, i32* %r0
	%r2 = sub i32 0, 23
	store i32 %r2, i32* %r1
	%r3 = load i32, i32* %r0
	%r4 = load i32, i32* %r1
	%r5 = add i32 %r3, %r4
	call void @printInt(i32 %r5)
	%r6 = load i32, i32* %r0
	%r7 = load i32, i32* %r1
	%r8 = sub i32 %r6, %r7
	call void @printInt(i32 %r8)
	%r9 = load i32, i32* %r0
	%r10 = load i32, i32* %r1
	%r11 = mul i32 %r9, %r10
	call void @printInt(i32 %r11)
	%r12 = sdiv i32 45, 2
	call void @printInt(i32 %r12)
	%r13 = srem i32 78, 3
	call void @printInt(i32 %r13)
	%r14 = load i32, i32* %r0
	%r15 = load i32, i32* %r1
	%r16 = sub i32 %r14, %r15
	%r17 = load i32, i32* %r0
	%r18 = load i32, i32* %r1
	%r19 = add i32 %r17, %r18
	%r20 = icmp sgt i32 %r16, %r19
	call void @printBool(i1 %r20)
	%r21 = load i32, i32* %r0
	%r22 = load i32, i32* %r1
	%r23 = sdiv i32 %r21, %r22
	%r24 = load i32, i32* %r0
	%r25 = load i32, i32* %r1
	%r26 = mul i32 %r24, %r25
	%r27 = icmp sle i32 %r23, %r26
	call void @printBool(i1 %r27)
	%r28 = bitcast [7 x i8]* @s1 to i8*
	%r29 = bitcast [2 x i8]* @s2 to i8*
	%r30 = call i8* @__concatStrings(i8* %r28,i8* %r29)
	%r31 = bitcast [14 x i8]* @s3 to i8*
	%r32 = call i8* @__concatStrings(i8* %r30,i8* %r31)
	call void @printString(i8* %r32)
	ret i32 0
}

define void @printBool(i1 %b) {
L0:
	%r0 = alloca i1
	store i1 %b, i1* %r0
	%r1 = load i1, i1* %r0
	br i1 %r1, label %L1, label %L2
L1:
	%r2 = bitcast [5 x i8]* @s4 to i8*
	call void @printString(i8* %r2)
	ret void
L2:
	%r3 = bitcast [6 x i8]* @s5 to i8*
	call void @printString(i8* %r3)
	ret void
L3:
	ret void
}

