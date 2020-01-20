declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s1 = private constant [1 x i8] c"\00"
@s4 = private constant [9 x i8] c"/* world\00"
@s2 = private constant [2 x i8] c"=\00"
@s3 = private constant [9 x i8] c"hello */\00"






define i32 @main() {
L0:
	%r0 = bitcast [1 x i8]* @s1 to i8*
	%r1 = alloca i8*
	store i8* %r0, i8** %r1
	%r2 = alloca i32
	store i32 0, i32* %r2
	%r3 = alloca i32
	store i32 0, i32* %r3
	%r4 = call i32 @fac(i32 10)
	call void @printInt(i32 %r4)
	%r5 = call i32 @rfac(i32 10)
	call void @printInt(i32 %r5)
	%r6 = call i32 @mfac(i32 10)
	call void @printInt(i32 %r6)
	%r7 = call i32 @ifac(i32 10)
	call void @printInt(i32 %r7)
	store i32 10, i32* %r2
	store i32 1, i32* %r3
	br label %L1
L1:
	%r8 = load i32, i32* %r2
	%r9 = icmp sgt i32 %r8, 0
	br i1 %r9, label %L2, label %L3
L2:
	%r10 = load i32, i32* %r3
	%r11 = load i32, i32* %r2
	%r12 = mul i32 %r10, %r11
	store i32 %r12, i32* %r3
	%r13 = load i32, i32* %r2
	%r14 = sub i32 %r13, 1
	store i32 %r14, i32* %r2
	br label %L1
L3:
	%r15 = load i32, i32* %r3
	call void @printInt(i32 %r15)
	%r16 = bitcast [2 x i8]* @s2 to i8*
	%r17 = call i8* @repStr(i8* %r16,i32 60)
	call void @printString(i8* %r17)
	%r18 = bitcast [9 x i8]* @s3 to i8*
	call void @printString(i8* %r18)
	%r19 = bitcast [9 x i8]* @s4 to i8*
	call void @printString(i8* %r19)
	ret i32 0
}

define i32 @fac(i32 %a) {
L0:
	%r0 = alloca i32
	store i32 %a, i32* %r0
	%r1 = alloca i32
	store i32 0, i32* %r1
	%r2 = alloca i32
	store i32 0, i32* %r2
	store i32 1, i32* %r1
	%r3 = load i32, i32* %r0
	store i32 %r3, i32* %r2
	br label %L1
L1:
	%r4 = load i32, i32* %r2
	%r5 = icmp sgt i32 %r4, 0
	br i1 %r5, label %L2, label %L3
L2:
	%r6 = load i32, i32* %r1
	%r7 = load i32, i32* %r2
	%r8 = mul i32 %r6, %r7
	store i32 %r8, i32* %r1
	%r9 = load i32, i32* %r2
	%r10 = sub i32 %r9, 1
	store i32 %r10, i32* %r2
	br label %L1
L3:
	%r11 = load i32, i32* %r1
	ret i32 %r11
}

define i32 @rfac(i32 %n) {
L0:
	%r0 = alloca i32
	store i32 %n, i32* %r0
	%r1 = load i32, i32* %r0
	%r2 = icmp eq i32 %r1, 0
	br i1 %r2, label %L1, label %L2
L1:
	ret i32 1
L2:
	%r3 = load i32, i32* %r0
	%r4 = load i32, i32* %r0
	%r5 = sub i32 %r4, 1
	%r6 = call i32 @rfac(i32 %r5)
	%r7 = mul i32 %r3, %r6
	ret i32 %r7
}

define i32 @mfac(i32 %n) {
L0:
	%r0 = alloca i32
	store i32 %n, i32* %r0
	%r1 = load i32, i32* %r0
	%r2 = icmp eq i32 %r1, 0
	br i1 %r2, label %L1, label %L2
L1:
	ret i32 1
L2:
	%r3 = load i32, i32* %r0
	%r4 = load i32, i32* %r0
	%r5 = sub i32 %r4, 1
	%r6 = call i32 @nfac(i32 %r5)
	%r7 = mul i32 %r3, %r6
	ret i32 %r7
}

define i32 @nfac(i32 %n) {
L0:
	%r0 = alloca i32
	store i32 %n, i32* %r0
	%r1 = load i32, i32* %r0
	%r2 = icmp ne i32 %r1, 0
	br i1 %r2, label %L1, label %L2
L1:
	%r3 = load i32, i32* %r0
	%r4 = sub i32 %r3, 1
	%r5 = call i32 @mfac(i32 %r4)
	%r6 = load i32, i32* %r0
	%r7 = mul i32 %r5, %r6
	ret i32 %r7
L2:
	ret i32 1
}

define i32 @ifac(i32 %n) {
L0:
	%r0 = alloca i32
	store i32 %n, i32* %r0
	%r1 = load i32, i32* %r0
	%r2 = call i32 @ifac2f(i32 1,i32 %r1)
	ret i32 %r2
}

define i32 @ifac2f(i32 %l, i32 %h) {
L0:
	%r0 = alloca i32
	store i32 %l, i32* %r0
	%r1 = alloca i32
	store i32 %h, i32* %r1
	%r2 = alloca i32
	store i32 0, i32* %r2
	%r3 = load i32, i32* %r0
	%r4 = load i32, i32* %r1
	%r5 = icmp eq i32 %r3, %r4
	br i1 %r5, label %L1, label %L2
L1:
	%r6 = load i32, i32* %r0
	ret i32 %r6
L2:
	%r7 = load i32, i32* %r0
	%r8 = load i32, i32* %r1
	%r9 = icmp sgt i32 %r7, %r8
	br i1 %r9, label %L3, label %L4
L3:
	ret i32 1
L4:
	%r10 = load i32, i32* %r0
	%r11 = load i32, i32* %r1
	%r12 = add i32 %r10, %r11
	%r13 = sdiv i32 %r12, 2
	store i32 %r13, i32* %r2
	%r14 = load i32, i32* %r0
	%r15 = load i32, i32* %r2
	%r16 = call i32 @ifac2f(i32 %r14,i32 %r15)
	%r17 = load i32, i32* %r2
	%r18 = add i32 %r17, 1
	%r19 = load i32, i32* %r1
	%r20 = call i32 @ifac2f(i32 %r18,i32 %r19)
	%r21 = mul i32 %r16, %r20
	ret i32 %r21
}

define i8* @repStr(i8* %s, i32 %n) {
L0:
	%r0 = alloca i8*
	store i8* %s, i8** %r0
	%r1 = alloca i32
	store i32 %n, i32* %r1
	%r2 = bitcast [1 x i8]* @s1 to i8*
	%r3 = alloca i8*
	store i8* %r2, i8** %r3
	%r4 = alloca i32
	store i32 0, i32* %r4
	%r5 = bitcast [1 x i8]* @s1 to i8*
	store i8* %r5, i8** %r3
	store i32 0, i32* %r4
	br label %L1
L1:
	%r6 = load i32, i32* %r4
	%r7 = load i32, i32* %r1
	%r8 = icmp slt i32 %r6, %r7
	br i1 %r8, label %L2, label %L3
L2:
	%r9 = load i8*, i8** %r3
	%r10 = load i8*, i8** %r0
	%r11 = call i8* @__concatStrings(i8* %r9,i8* %r10)
	store i8* %r11, i8** %r3
	%r12 = load i32, i32* %r4
	%r13 = add i32 %r12, 1
	store i32 %r13, i32* %r4
	br label %L1
L3:
	%r14 = load i8*, i8** %r3
	ret i8* %r14
}

