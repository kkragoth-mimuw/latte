declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)



define i32 @f() {
L0:
	ret i32 0
}

define i32 @g() {
L0:
	ret i32 0
}

define void @p() {
L0:
	ret void
}

define i32 @main() {
L0:
	call void @p()
	ret i32 0
}

