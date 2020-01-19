declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s1 = private constant [2 x i8] c"4\00"
@s2 = private constant [2 x i8] c"6\00"






define i32 @main() {
L0:
	%r0 = bitcast [2 x i8]* @s1 to i8*
	call void @printString(i8* %r0)
	%r1 = bitcast [2 x i8]* @s1 to i8*
	call void @printString(i8* %r1)
	%r2 = bitcast [2 x i8]* @s2 to i8*
	call void @printString(i8* %r2)
	%r3 = bitcast [2 x i8]* @s2 to i8*
	call void @printString(i8* %r3)
	ret i32 0
}

