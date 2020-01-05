declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)



define i32 @main() {
L0:
	br i1 1, label %L1, label %L2
L1:
	call void @printInt(i32 1)
	ret i32 0
}

