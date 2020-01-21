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
	store i32 0, i32* %r0
	store i32 1, i32* %r1
	store i32 2, i32* %r2
	store i32 3, i32* %r3
	store i32 4, i32* %r4
	br label %L1
L1:
	%r5 = load i32, i32* %r0
	%r6 = icmp slt i32 %r5, 10
	br i1 %r6, label %L2, label %L3
L2:
	%r7 = load i32, i32* %r0
	%r8 = add i32 %r7, 1
	store i32 %r8, i32* %r0
	br label %L4
L3:
	%r25 = load i32, i32* %r0
	call void @printInt(i32 %r25)
	%r26 = load i32, i32* %r1
	call void @printInt(i32 %r26)
	%r27 = load i32, i32* %r2
	call void @printInt(i32 %r27)
	%r28 = load i32, i32* %r3
	call void @printInt(i32 %r28)
	%r29 = load i32, i32* %r4
	call void @printInt(i32 %r29)
	%r30 = load i32, i32* %r0
	%r31 = load i32, i32* %r1
	%r32 = add i32 %r30, %r31
	%r33 = load i32, i32* %r2
	%r34 = add i32 %r32, %r33
	%r35 = load i32, i32* %r3
	%r36 = add i32 %r34, %r35
	%r37 = load i32, i32* %r4
	%r38 = add i32 %r36, %r37
	call void @printInt(i32 %r38)
	ret i32 0
L4:
	%r9 = load i32, i32* %r1
	%r10 = icmp slt i32 %r9, 10
	br i1 %r10, label %L5, label %L1
L5:
	br label %L7
L7:
	%r11 = load i32, i32* %r2
	%r12 = icmp slt i32 %r11, 10
	br i1 %r12, label %L8, label %L9
L8:
	%r13 = load i32, i32* %r2
	%r14 = add i32 %r13, 1
	store i32 %r14, i32* %r2
	br label %L10
L9:
	%r23 = load i32, i32* %r1
	%r24 = add i32 %r23, 1
	store i32 %r24, i32* %r1
	br label %L4
L10:
	%r15 = load i32, i32* %r3
	%r16 = icmp slt i32 %r15, 10
	br i1 %r16, label %L11, label %L7
L11:
	br label %L13
L13:
	%r17 = load i32, i32* %r4
	%r18 = icmp slt i32 %r17, 10
	br i1 %r18, label %L14, label %L15
L14:
	%r19 = load i32, i32* %r4
	%r20 = add i32 %r19, 1
	store i32 %r20, i32* %r4
	br label %L13
L15:
	%r21 = load i32, i32* %r3
	%r22 = add i32 %r21, 1
	store i32 %r22, i32* %r3
	br label %L10
}

