declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s3 = private constant [2 x i8] c"!\00"
@s1 = private constant [3 x i8] c"&&\00"
@s4 = private constant [6 x i8] c"false\00"
@s5 = private constant [5 x i8] c"true\00"
@s2 = private constant [3 x i8] c"||\00"

define i32 @main() {
L0:
	%r0 = bitcast [3 x i8]* @s1 to i8*
	call void @printString(i8* %r0)
	%r1 = sub i32 0, 1
	%r2 = call i1 @test(i32 %r1)
	br i1 %r2, label %L1, label %L2
L1:
	%r3 = call i1 @test(i32 0)
	br label %L2
L2:
	%r4 = phi i1 [ %r2, %L0 ], [ %r3, %L1 ]
	call void @printBool(i1 %r4)
	%r5 = sub i32 0, 2
	%r6 = call i1 @test(i32 %r5)
	br i1 %r6, label %L3, label %L4
L3:
	%r7 = call i1 @test(i32 1)
	br label %L4
L4:
	%r8 = phi i1 [ %r6, %L2 ], [ %r7, %L3 ]
	call void @printBool(i1 %r8)
	%r9 = call i1 @test(i32 3)
	br i1 %r9, label %L5, label %L6
L5:
	%r10 = sub i32 0, 5
	%r11 = call i1 @test(i32 %r10)
	br label %L6
L6:
	%r12 = phi i1 [ %r9, %L4 ], [ %r11, %L5 ]
	call void @printBool(i1 %r12)
	%r13 = call i1 @test(i32 234234)
	br i1 %r13, label %L7, label %L8
L7:
	%r14 = call i1 @test(i32 21321)
	br label %L8
L8:
	%r15 = phi i1 [ %r13, %L6 ], [ %r14, %L7 ]
	call void @printBool(i1 %r15)
	%r16 = bitcast [3 x i8]* @s2 to i8*
	call void @printString(i8* %r16)
	%r17 = sub i32 0, 1
	%r18 = call i1 @test(i32 %r17)
	br i1 %r18, label %L10, label %L9
L9:
	%r19 = call i1 @test(i32 0)
	br label %L10
L10:
	%r20 = phi i1 [ %r18, %L8 ], [ %r19, %L9 ]
	call void @printBool(i1 %r20)
	%r21 = sub i32 0, 2
	%r22 = call i1 @test(i32 %r21)
	br i1 %r22, label %L12, label %L11
L11:
	%r23 = call i1 @test(i32 1)
	br label %L12
L12:
	%r24 = phi i1 [ %r22, %L10 ], [ %r23, %L11 ]
	call void @printBool(i1 %r24)
	%r25 = call i1 @test(i32 3)
	br i1 %r25, label %L14, label %L13
L13:
	%r26 = sub i32 0, 5
	%r27 = call i1 @test(i32 %r26)
	br label %L14
L14:
	%r28 = phi i1 [ %r25, %L12 ], [ %r27, %L13 ]
	call void @printBool(i1 %r28)
	%r29 = call i1 @test(i32 234234)
	br i1 %r29, label %L16, label %L15
L15:
	%r30 = call i1 @test(i32 21321)
	br label %L16
L16:
	%r31 = phi i1 [ %r29, %L14 ], [ %r30, %L15 ]
	call void @printBool(i1 %r31)
	%r32 = bitcast [2 x i8]* @s3 to i8*
	call void @printString(i8* %r32)
	call void @printBool(i1 1)
	call void @printBool(i1 0)
	ret i32 0
}

define void @printBool(i1 %b) {
L0:
	%r0 = alloca i1
	store i1 %b, i1* %r0
	%r1 = load i1, i1* %r0
	%r2 = xor i1 1, %r1
	br i1 %r2, label %L1, label %L2
L1:
	%r3 = bitcast [6 x i8]* @s4 to i8*
	call void @printString(i8* %r3)
	br label %L3
L2:
	%r4 = bitcast [5 x i8]* @s5 to i8*
	call void @printString(i8* %r4)
	br label %L3
L3:
	ret void
}

define i1 @test(i32 %i) {
L0:
	%r0 = alloca i32
	store i32 %i, i32* %r0
	%r1 = load i32, i32* %r0
	call void @printInt(i32 %r1)
	%r2 = load i32, i32* %r0
	%r3 = icmp sgt i32 %r2, 0
	ret i1 %r3
}

