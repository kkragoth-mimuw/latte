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
	%r0 = call i32 @fac(i32 5)
	call void @printInt(i32 %r0)
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

