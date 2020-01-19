declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)






define i32 @main() {
L0:
	%r0 = call i32 @readInt()
	%r1 = alloca i32
	store i32 %r0, i32* %r1
	%r2 = call i8* @readString()
	%r3 = alloca i8*
	store i8* %r2, i8** %r3
	%r4 = call i8* @readString()
	%r5 = alloca i8*
	store i8* %r4, i8** %r5
	%r6 = load i32, i32* %r1
	%r7 = sub i32 %r6, 5
	call void @printInt(i32 %r7)
	%r8 = load i8*, i8** %r3
	%r9 = load i8*, i8** %r5
	%r10 = call i8* @__concatStrings(i8* %r8,i8* %r9)
	call void @printString(i8* %r10)
	ret i32 0
}

