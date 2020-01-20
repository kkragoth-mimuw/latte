declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s1 = private constant [1 x i8] c"\00"






define i32 @main() {
L0:
	%r0 = alloca i32
	store i32 0, i32* %r0
	%r1 = bitcast [1 x i8]* @s1 to i8*
	%r2 = alloca i8*
	store i8* %r1, i8** %r2
	%r3 = bitcast [1 x i8]* @s1 to i8*
	%r4 = alloca i8*
	store i8* %r3, i8** %r4
	%r5 = call i32 @readInt()
	store i32 %r5, i32* %r0
	%r6 = call i8* @readString()
	store i8* %r6, i8** %r2
	%r7 = call i8* @readString()
	store i8* %r7, i8** %r4
	%r8 = load i32, i32* %r0
	%r9 = sub i32 %r8, 5
	call void @printInt(i32 %r9)
	%r10 = load i8*, i8** %r2
	%r11 = load i8*, i8** %r4
	%r12 = call i8* @__concatStrings(i8* %r10,i8* %r11)
	call void @printString(i8* %r12)
	ret i32 0
}

