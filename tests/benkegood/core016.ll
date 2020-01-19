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
	store i32 17, i32* %r0
	br label %L1
L1:
	%r1 = load i32, i32* %r0
	%r2 = icmp sgt i32 %r1, 0
	br i1 %r2, label %L2, label %L3
L2:
	%r3 = load i32, i32* %r0
	%r4 = sub i32 %r3, 2
	store i32 %r4, i32* %r0
	br label %L1
L3:
	%r5 = load i32, i32* %r0
	%r6 = icmp slt i32 %r5, 0
	br i1 %r6, label %L4, label %L5
L4:
	call void @printInt(i32 0)
	ret i32 0
L5:
	call void @printInt(i32 1)
	ret i32 0
}

