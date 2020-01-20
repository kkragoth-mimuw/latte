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
	store i32 45, i32* %r0
	%r2 = sub i32 0, 36
	store i32 %r2, i32* %r1
	%r3 = load i32, i32* %r0
	call void @printInt(i32 %r3)
	%r4 = load i32, i32* %r1
	call void @printInt(i32 %r4)
	ret i32 0
}

