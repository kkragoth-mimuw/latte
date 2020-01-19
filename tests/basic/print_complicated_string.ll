declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s1 = private constant [9 x i8] c"\5Ca\5Cn\0A\09b\22\00"




define i32 @main() {
L0:
	%r0 = bitcast [9 x i8]* @s1 to i8*
	call void @printString(i8* %r0)
	ret i32 0
}

