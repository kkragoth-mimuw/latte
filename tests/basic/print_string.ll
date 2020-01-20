declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)

@s1 = private constant [4 x i8] c"abc\00"






define i32 @main() {
L0:
	%r0 = bitcast [4 x i8]* @s1 to i8*
	call void @printString(i8* %r0)
	ret i32 0
}

