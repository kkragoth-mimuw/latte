declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)

@s1 = private constant [5 x i8] c"ahoj\00"






define i32 @main() {
L0:
	%r0 = call i1 @print()
	br i1 %r0, label %L1, label %L2
L1:
	br label %L2
L2:
	%r1 = phi i1 [ %r0, %L0 ], [ 0, %L1 ]
	ret i32 0
}

define i1 @print() {
L0:
	%r0 = bitcast [5 x i8]* @s1 to i8*
	call void @printString(i8* %r0)
	ret i1 1
}

