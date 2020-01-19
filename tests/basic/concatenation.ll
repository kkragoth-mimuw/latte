declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s1 = private constant [2 x i8] c"a\00"
@s2 = private constant [2 x i8] c"b\00"






define i32 @main() {
L0:
	%r0 = bitcast [2 x i8]* @s1 to i8*
	%r1 = bitcast [2 x i8]* @s2 to i8*
	%r2 = call i8* @__concatStrings(i8* %r0,i8* %r1)
	call void @printString(i8* %r2)
	ret i32 0
}

