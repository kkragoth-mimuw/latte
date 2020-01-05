declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)



define i32 @f() {
L0:
	br i1 1, label %L1, label %L2
L1:
	ret i32 0
	br label %L3
L2:
	br label %L3
}

define i32 @g() {
L0:
	br i1 0, label %L1, label %L2
L1:
	br label %L3
L2:
	ret i32 0
	br label %L3
}

define void @p() {
L0:
	
}

define i32 @main() {
L0:
	call void @p()
	ret i32 0
}

