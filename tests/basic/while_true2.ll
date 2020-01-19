declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s1 = private constant [12 x i8] c"jeszcze raz\00"






define i32 @main() {
L0:
	br label %L1
L1:
	%r0 = alloca i32
	store i32 0, i32* %r0
	%r1 = call i32 @readInt()
	store i32 %r1, i32* %r0
	%r2 = load i32, i32* %r0
	%r3 = icmp eq i32 %r2, 1
	br i1 %r3, label %L2, label %L3
L2:
	ret i32 0
L3:
	%r4 = bitcast [12 x i8]* @s1 to i8*
	call void @printString(i8* %r4)
	br label %L1
}

