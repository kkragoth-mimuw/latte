declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)

@s1 = private constant [7 x i8] c"abcdef\00"






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
	store i32 50, i32* %r1
	store i32 2, i32* %r2
	store i32 4, i32* %r3
	store i32 5, i32* %r4
	store i32 6, i32* %r5
	store i32 0, i32* %r6
	%r7 = load i32, i32* %r0
	%r8 = load i32, i32* %r1
	%r9 = icmp slt i32 %r7, %r8
	br i1 %r9, label %L1, label %L2
L1:
	br label %L3
L2:
	br label %L8
L3:
	%r10 = load i32, i32* %r0
	%r11 = load i32, i32* %r1
	%r12 = icmp slt i32 %r10, %r11
	br i1 %r12, label %L4, label %L2
L4:
	%r13 = load i32, i32* %r3
	%r14 = load i32, i32* %r4
	%r15 = icmp slt i32 %r13, %r14
	br i1 %r15, label %L6, label %L7
L6:
	%r16 = load i32, i32* %r0
	%r17 = add i32 %r16, 1
	store i32 %r17, i32* %r0
	%r18 = load i32, i32* %r0
	call void @printInt(i32 %r18)
	br label %L3
L7:
	%r19 = load i32, i32* %r1
	%r20 = add i32 %r19, 1
	store i32 %r20, i32* %r1
	%r21 = load i32, i32* %r1
	call void @printInt(i32 %r21)
	br label %L3
L8:
	%r22 = load i32, i32* %r0
	%r23 = load i32, i32* %r1
	%r24 = add i32 %r22, %r23
	%r25 = load i32, i32* %r2
	%r26 = add i32 %r24, %r25
	%r27 = load i32, i32* %r3
	%r28 = add i32 %r26, %r27
	%r29 = load i32, i32* %r4
	%r30 = add i32 %r28, %r29
	%r31 = load i32, i32* %r5
	%r32 = add i32 %r30, %r31
	%r33 = icmp slt i32 %r32, 150
	br i1 %r33, label %L9, label %L10
L9:
	%r34 = load i32, i32* %r0
	%r35 = load i32, i32* %r2
	%r36 = load i32, i32* %r3
	%r37 = add i32 %r35, %r36
	%r38 = load i32, i32* %r4
	%r39 = add i32 %r37, %r38
	%r40 = icmp slt i32 %r34, %r39
	br i1 %r40, label %L11, label %L12
L10:
	%r59 = load i32, i32* %r0
	%r60 = load i32, i32* %r1
	%r61 = icmp slt i32 %r59, %r60
	br i1 %r61, label %L13, label %L14
L11:
	%r41 = load i32, i32* %r0
	%r42 = add i32 %r41, 1
	store i32 %r42, i32* %r0
	br label %L12
L12:
	%r43 = load i32, i32* %r1
	%r44 = add i32 %r43, 1
	store i32 %r44, i32* %r1
	%r45 = load i32, i32* %r2
	%r46 = add i32 %r45, 1
	store i32 %r46, i32* %r2
	%r47 = bitcast [7 x i8]* @s1 to i8*
	call void @printString(i8* %r47)
	%r48 = load i32, i32* %r0
	%r49 = load i32, i32* %r1
	%r50 = add i32 %r48, %r49
	%r51 = load i32, i32* %r2
	%r52 = add i32 %r50, %r51
	%r53 = load i32, i32* %r3
	%r54 = add i32 %r52, %r53
	%r55 = load i32, i32* %r4
	%r56 = add i32 %r54, %r55
	%r57 = load i32, i32* %r5
	%r58 = add i32 %r56, %r57
	call void @printInt(i32 %r58)
	br label %L8
L13:
	%r62 = load i32, i32* %r0
	call void @printInt(i32 %r62)
	br label %L15
L14:
	br label %L16
L15:
	%r75 = load i32, i32* %r0
	call void @printInt(i32 %r75)
	ret i32 0
L16:
	%r63 = load i32, i32* %r6
	%r64 = icmp slt i32 %r63, 100
	br i1 %r64, label %L17, label %L15
L17:
	%r65 = load i32, i32* %r6
	%r66 = add i32 %r65, 1
	store i32 %r66, i32* %r6
	%r67 = load i32, i32* %r6
	%r68 = icmp sgt i32 %r67, 30
	br i1 %r68, label %L19, label %L16
L19:
	%r69 = load i32, i32* %r0
	call void @printInt(i32 %r69)
	%r70 = load i32, i32* %r1
	call void @printInt(i32 %r70)
	%r71 = load i32, i32* %r2
	call void @printInt(i32 %r71)
	%r72 = load i32, i32* %r6
	%r73 = load i32, i32* %r6
	%r74 = add i32 %r72, %r73
	store i32 %r74, i32* %r6
	br label %L16
}

