declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s1 = private constant [4 x i8] c"apa\00"
@s3 = private constant [6 x i8] c"false\00"
@s2 = private constant [5 x i8] c"true\00"

define i32 @main() {
L0:
	%r0 = alloca i32
	store i32 4, i32* %r0
	%r1 = load i32, i32* %r0
	%r2 = icmp sle i32 3, %r1
	br i1 %r2, label %L1, label %L4
L1:
	br i1 1, label %L2, label %L3
L2:
	br label %L3
L3:
	%r3 = phi i1 [ 1, %L1 ], [ 1, %L2 ]
	br label %L4
L4:
	%r4 = phi i1 [ %r2, %L0 ], [ %r3, %L3 ]
	br i1 %r4, label %L5, label %L6
L5:
	call void @printBool(i1 1)
	br label %L7
L6:
	%r5 = bitcast [4 x i8]* @s1 to i8*
	call void @printString(i8* %r5)
	br label %L7
L7:
	%r6 = icmp eq i1 1, 1
	br i1 %r6, label %L9, label %L8
L8:
	%r7 = call i1 @dontCallMe(i32 1)
	br label %L9
L9:
	%r8 = phi i1 [ %r6, %L7 ], [ %r7, %L8 ]
	call void @printBool(i1 %r8)
	%r9 = sub i32 0, 5
	%r10 = icmp slt i32 4, %r9
	br i1 %r10, label %L10, label %L11
L10:
	%r11 = call i1 @dontCallMe(i32 2)
	br label %L11
L11:
	%r12 = phi i1 [ %r10, %L9 ], [ %r11, %L10 ]
	call void @printBool(i1 %r12)
	%r13 = load i32, i32* %r0
	%r14 = icmp eq i32 4, %r13
	br i1 %r14, label %L12, label %L15
L12:
	%r15 = xor i1 1, 0
	%r16 = icmp eq i1 1, %r15
	br i1 %r16, label %L13, label %L14
L13:
	br label %L14
L14:
	%r17 = phi i1 [ %r16, %L12 ], [ 1, %L13 ]
	br label %L15
L15:
	%r18 = phi i1 [ %r14, %L11 ], [ %r17, %L14 ]
	call void @printBool(i1 %r18)
	%r19 = call i1 @implies(i1 0,i1 0)
	call void @printBool(i1 %r19)
	%r20 = call i1 @implies(i1 0,i1 1)
	call void @printBool(i1 %r20)
	%r21 = call i1 @implies(i1 1,i1 0)
	call void @printBool(i1 %r21)
	%r22 = call i1 @implies(i1 1,i1 1)
	call void @printBool(i1 %r22)
	ret i32 0
}

define i1 @dontCallMe(i32 %x) {
L0:
	%r0 = alloca i32
	store i32 %x, i32* %r0
	%r1 = load i32, i32* %r0
	call void @printInt(i32 %r1)
	ret i1 1
}

define void @printBool(i1 %b) {
L0:
	%r0 = alloca i1
	store i1 %b, i1* %r0
	%r1 = load i1, i1* %r0
	br i1 %r1, label %L1, label %L2
L1:
	%r2 = bitcast [5 x i8]* @s2 to i8*
	call void @printString(i8* %r2)
	br label %L3
L2:
	%r3 = bitcast [6 x i8]* @s3 to i8*
	call void @printString(i8* %r3)
	br label %L3
L3:
	ret void
}

define i1 @implies(i1 %x, i1 %y) {
L0:
	%r0 = alloca i1
	store i1 %x, i1* %r0
	%r1 = alloca i1
	store i1 %y, i1* %r1
	%r2 = load i1, i1* %r0
	%r3 = xor i1 1, %r2
	br i1 %r3, label %L2, label %L1
L1:
	%r4 = load i1, i1* %r0
	%r5 = load i1, i1* %r1
	%r6 = icmp eq i1 %r4, %r5
	br label %L2
L2:
	%r7 = phi i1 [ %r3, %L0 ], [ %r6, %L1 ]
	ret i1 %r7
}

