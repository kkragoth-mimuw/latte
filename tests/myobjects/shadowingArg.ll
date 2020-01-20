declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)




%foo = type {
	i32
}



define void @foo__printBar(%foo* %self, i32 %bar) {
L0:
	%r0 = alloca %foo*
	store %foo* %self, %foo** %r0
	%r1 = alloca i32
	store i32 %bar, i32* %r1
	%r2 = load i32, i32* %r1
	call void @printInt(i32 %r2)
	%r3 = load %foo*, %foo** %r0
	%r4 = getelementptr %foo, %foo* %r3, i32 0, i32 0
	%r5 = load i32, i32* %r4
	call void @printInt(i32 %r5)
	ret void
}


define i32 @main() {
L0:
	%r0 = alloca %foo*
	store %foo* null, %foo** %r0
	%r1 = call i8* @malloc(i32 4)
	%r2 = bitcast i8* %r1 to %foo*
	%r3 = getelementptr %foo, %foo* %r2, i32 0, i32 0
	store i32 0, i32* %r3
	store %foo* %r2, %foo** %r0
	%r4 = load %foo*, %foo** %r0
	%r5 = getelementptr %foo, %foo* %r4, i32 0, i32 0
	store i32 3, i32* %r5
	%r6 = load %foo*, %foo** %r0
	%r7 = bitcast %foo* %r6 to %foo*
	call void @foo__printBar(%foo* %r7,i32 2)
	ret i32 0
}

