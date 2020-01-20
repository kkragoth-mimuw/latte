declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)

@s1 = private constant [1 x i8] c"\00"
@s2 = private constant [6 x i8] c"rowne\00"






define i32 @main() {
L0:
	%r0 = bitcast [1 x i8]* @s1 to i8*
	%r1 = alloca i8*
	store i8* %r0, i8** %r1
	%r2 = bitcast [1 x i8]* @s1 to i8*
	%r3 = alloca i8*
	store i8* %r2, i8** %r3
	%r4 = load i8*, i8** %r1
	%r5 = load i8*, i8** %r3
	%r6 = call i1 @__compareStringsEQ(i8* %r4,i8* %r5)
	br i1 %r6, label %L1, label %L2
L1:
	%r7 = bitcast [6 x i8]* @s2 to i8*
	call void @printString(i8* %r7)
	br label %L2
L2:
	ret i32 0
}

