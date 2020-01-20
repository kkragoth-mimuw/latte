declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s1 = private constant [157 x i8] c"\22\0Apop\0Apowrot:\0Agetstatic java/lang/System/out Ljava/io/PrintStream;\0Aldc \22zle \22\0Ainvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\0Agoto powrot\0Aldc \22\00"






define i32 @f(i32 %p) {
L0:
	%r0 = alloca i32
	store i32 %p, i32* %r0
	%r1 = alloca i32
	store i32 0, i32* %r1
	%r2 = load i32, i32* %r0
	%r3 = load i32, i32* %r0
	%r4 = mul i32 2, %r3
	%r5 = add i32 %r2, %r4
	store i32 %r5, i32* %r1
	%r6 = bitcast [157 x i8]* @s1 to i8*
	call void @printString(i8* %r6)
	%r7 = load i32, i32* %r1
	ret i32 %r7
}

define i32 @main() {
L0:
	%r0 = call i32 @f(i32 1)
	%r1 = sub i32 %r0, 3
	ret i32 %r1
}

