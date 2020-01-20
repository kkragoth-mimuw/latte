declare i8* @malloc(i32) nounwind
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
	%r1 = call i32 @foo()
	store i32 %r1, i32* %r0
	%r2 = load i32, i32* %r0
	call void @printInt(i32 %r2)
	ret i32 0
}

define i32 @foo() {
L0:
	ret i32 10
}

