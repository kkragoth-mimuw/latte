declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)



define i32 @main() {
L0:
	%r0 = call i32 @ev(i32 17)
	call void @printInt(i32 %r0)
	ret i32 0
}

define i32 @ev(i32 %y) {
L0:
	%r0 = alloca i32
	store i32 %y, i32* %r0
	%r1 = load i32, i32* %r0
	%r2 = icmp sgt i32 %r1, 0
	br i1 %r2, label %L1, label %L2
L1:
	%r3 = load i32, i32* %r0
	%r4 = sub i32 %r3, 2
	%r5 = call i32 @ev(i32 %r4)
	ret i32 %r5
L2:
	%r6 = load i32, i32* %r0
	%r7 = icmp slt i32 %r6, 0
	br i1 %r7, label %L3, label %L4
L3:
	ret i32 0
L4:
	ret i32 1
}

