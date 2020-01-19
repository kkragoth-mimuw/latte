declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)




%point2 = type {
	i32,
	i32
}



define void @point2__printX(%point2* %this) {
L0:
	%r0 = alloca %point2*
	store %point2* %this, %point2** %r0
	call void @printInt(i32 2)
	ret void
}


define i32 @main() {
L0:
	%r0 = call i8* @malloc(i32 8)
	%r1 = bitcast i8* %r0 to %point2*
	%r2 = alloca %point2*
	store %point2* %r1, %point2** %r2
	%r3 = load %point2*, %point2** %r2
	%r4 = getelementptr %point2, %point2* %r3, i32 0, i32 0
	store i32 2, i32* %r4
	%r5 = load %point2*, %point2** %r2
	call void @point2__printX(%point2* %r5)
	ret i32 0
}

