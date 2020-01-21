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
@s2 = private constant [7 x i8] c"hello \00"
@s4 = private constant [12 x i8] c"hello world\00"
@s5 = private constant [3 x i8] c"ok\00"
@s3 = private constant [6 x i8] c"world\00"






define i32 @main() {
L0:
	%r0 = bitcast [1 x i8]* @s1 to i8*
	%r1 = alloca i8*
	store i8* %r0, i8** %r1
	%r2 = bitcast [1 x i8]* @s1 to i8*
	%r3 = alloca i8*
	store i8* %r2, i8** %r3
	%r4 = bitcast [7 x i8]* @s2 to i8*
	store i8* %r4, i8** %r1
	%r5 = bitcast [6 x i8]* @s3 to i8*
	store i8* %r5, i8** %r3
	%r6 = load i8*, i8** %r1
	%r7 = load i8*, i8** %r3
	%r8 = call i8* @__concatStrings(i8* %r6,i8* %r7)
	%r9 = bitcast [12 x i8]* @s4 to i8*
	%r10 = call i1 @__compareStringsEQ(i8* %r8,i8* %r9)
	br i1 %r10, label %L1, label %L2
L1:
	%r11 = bitcast [3 x i8]* @s5 to i8*
	call void @printString(i8* %r11)
	br label %L2
L2:
	ret i32 0
}

