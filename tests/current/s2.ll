declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s1 = private constant [15 x i8] c"a.next is null\00"


%list = type {
	i32,
	%list*,
	i8*
}


define i32 @main() {
L0:
	%r0 = call i8* @malloc(i32 20)
	%r1 = bitcast i8* %r0 to %list*
	%r2 = alloca %list*
	store %list* %r1, %list** %r2
	%r3 = load %list*, %list** %r2
	%r4 = getelementptr %list, %list* %r3, i32 0, i32 1
	%r5 = load %list*, %list** %r4
	%r6 = icmp eq %list* %r5, null
	br i1 %r6, label %L1, label %L2
L1:
	%r7 = bitcast [15 x i8]* @s1 to i8*
	call void @printString(i8* %r7)
	br label %L2
L2:
	ret i32 0
}

