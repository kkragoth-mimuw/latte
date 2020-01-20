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
	%r0 = srem i32 5, 3
	call void @printInt(i32 %r0)
	%r1 = sub i32 0, 5
	%r2 = srem i32 %r1, 3
	call void @printInt(i32 %r2)
	ret i32 0
}

