declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s1 = private constant [8 x i8] c"a is ok\00"
@s2 = private constant [11 x i8] c"null is ok\00"


%list = type {
	i32,
	%list*
}




define i32 @main() {
L0:
	%r0 = call i8* @malloc(i32 12)
	%r1 = bitcast i8* %r0 to %list*
	%r2 = getelementptr %list, %list* %r1, i32 0, i32 0
	store i32 0, i32* %r2
	%r3 = getelementptr %list, %list* %r1, i32 0, i32 1
	store %list* null, %list** %r3
	%r4 = alloca %list*
	store %list* %r1, %list** %r4
	%r5 = load %list*, %list** %r4
	%r6 = getelementptr %list, %list* %r5, i32 0, i32 0
	%r7 = load i32, i32* %r6
	%r8 = icmp eq i32 %r7, 0
	br i1 %r8, label %L1, label %L2
L1:
	%r9 = bitcast [8 x i8]* @s1 to i8*
	call void @printString(i8* %r9)
	br label %L2
L2:
	%r10 = load %list*, %list** %r4
	%r11 = getelementptr %list, %list* %r10, i32 0, i32 1
	%r12 = load %list*, %list** %r11
	%r13 = icmp eq %list* %r12, null
	br i1 %r13, label %L3, label %L4
L3:
	%r14 = bitcast [11 x i8]* @s2 to i8*
	call void @printString(i8* %r14)
	br label %L4
L4:
	ret i32 0
}

