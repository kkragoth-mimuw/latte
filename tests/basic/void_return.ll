declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)








define i32 @main() {
L0:
	call void @run()
	ret i32 0
}

define void @run() {
L0:
	call void @printInt(i32 0)
	ret void
}

