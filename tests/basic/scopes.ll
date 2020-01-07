declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)



define i32 @main() {
L0:
	%r0 = alloca i32
	store i32 0, i32* %r0
	%r1 = load i32, i32* %r0
	call void @printInt(i32 %r1)
	%r2 = alloca i32
	store i32 1, i32* %r2
	%r3 = load i32, i32* %r2
	call void @printInt(i32 %r3)
	%r4 = load i32, i32* %r0
	call void @printInt(i32 %r4)
	%r5 = alloca i32
	store i32 2, i32* %r5
	%r6 = load i32, i32* %r5
	call void @printInt(i32 %r6)
	%r7 = load i32, i32* %r0
	call void @printInt(i32 %r7)
	ret i32 0
}

