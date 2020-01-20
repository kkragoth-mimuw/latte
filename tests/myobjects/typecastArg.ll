declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)

@s1 = private constant [3 x i8] c"OK\00"


%A = type {
	i32
}

%B = type {
	i32,
	i32
}





define void @shouldWork(%A* %a) {
L0:
	%r0 = alloca %A*
	store %A* %a, %A** %r0
	%r1 = bitcast [3 x i8]* @s1 to i8*
	call void @printString(i8* %r1)
	ret void
}

define i32 @main() {
L0:
	%r0 = alloca %B*
	store %B* null, %B** %r0
	%r1 = call i8* @malloc(i32 8)
	%r2 = bitcast i8* %r1 to %B*
	%r3 = getelementptr %B, %B* %r2, i32 0, i32 0
	store i32 0, i32* %r3
	%r4 = getelementptr %B, %B* %r2, i32 0, i32 1
	store i32 0, i32* %r4
	store %B* %r2, %B** %r0
	%r5 = load %B*, %B** %r0
	%r6 = bitcast %B* %r5 to %A*
	call void @shouldWork(%A* %r6)
	ret i32 0
}

