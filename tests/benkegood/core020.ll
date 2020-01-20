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
	call void @p()
	call void @printInt(i32 1)
	ret i32 0
}

define void @p() {
L0:
	ret void
}

