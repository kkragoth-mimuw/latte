declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)

@s2 = private constant [6 x i8] c"false\00"
@s1 = private constant [5 x i8] c"true\00"






define i32 @main() {
L0:
	%r0 = call i1 @t(i32 1)
	br i1 %r0, label %L1, label %L2
L1:
	%r1 = call i1 @f(i32 2)
	br label %L2
L2:
	%r2 = phi i1 [ %r0, %L0 ], [ %r1, %L1 ]
	call void @b(i1 %r2)
	%r3 = call i1 @t(i32 3)
	br i1 %r3, label %L3, label %L4
L3:
	%r4 = call i1 @t(i32 4)
	br label %L4
L4:
	%r5 = phi i1 [ %r3, %L2 ], [ %r4, %L3 ]
	call void @b(i1 %r5)
	%r6 = call i1 @t(i32 5)
	br i1 %r6, label %L6, label %L5
L5:
	%r7 = call i1 @t(i32 6)
	br label %L6
L6:
	%r8 = phi i1 [ %r6, %L4 ], [ %r7, %L5 ]
	call void @b(i1 %r8)
	%r9 = call i1 @f(i32 7)
	br i1 %r9, label %L7, label %L8
L7:
	%r10 = call i1 @t(i32 8)
	br label %L8
L8:
	%r11 = phi i1 [ %r9, %L6 ], [ %r10, %L7 ]
	call void @b(i1 %r11)
	%r12 = call i1 @t(i32 9)
	br i1 %r12, label %L9, label %L12
L9:
	%r13 = call i1 @t(i32 10)
	br i1 %r13, label %L10, label %L11
L10:
	%r14 = call i1 @t(i32 11)
	br label %L11
L11:
	%r15 = phi i1 [ %r13, %L9 ], [ %r14, %L10 ]
	br label %L12
L12:
	%r16 = phi i1 [ %r12, %L8 ], [ %r15, %L11 ]
	call void @b(i1 %r16)
	%r17 = call i1 @f(i32 12)
	br i1 %r17, label %L16, label %L13
L13:
	%r18 = call i1 @f(i32 13)
	br i1 %r18, label %L14, label %L15
L14:
	%r19 = call i1 @t(i32 14)
	br label %L15
L15:
	%r20 = phi i1 [ %r18, %L13 ], [ %r19, %L14 ]
	br label %L16
L16:
	%r21 = phi i1 [ %r17, %L12 ], [ %r20, %L15 ]
	call void @b(i1 %r21)
	ret i32 0
}

define i1 @f(i32 %a) {
L0:
	%r0 = alloca i32
	store i32 %a, i32* %r0
	%r1 = load i32, i32* %r0
	call void @printInt(i32 %r1)
	ret i1 0
}

define i1 @t(i32 %a) {
L0:
	%r0 = alloca i32
	store i32 %a, i32* %r0
	%r1 = load i32, i32* %r0
	%r2 = call i1 @f(i32 %r1)
	%r3 = xor i1 1, %r2
	ret i1 %r3
}

define void @b(i1 %a) {
L0:
	%r0 = alloca i1
	store i1 %a, i1* %r0
	%r1 = load i1, i1* %r0
	br i1 %r1, label %L1, label %L2
L1:
	%r2 = bitcast [5 x i8]* @s1 to i8*
	call void @printString(i8* %r2)
	br label %L3
L2:
	%r3 = bitcast [6 x i8]* @s2 to i8*
	call void @printString(i8* %r3)
	br label %L3
L3:
	ret void
}

