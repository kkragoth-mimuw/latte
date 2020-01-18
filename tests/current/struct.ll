declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s1 = private constant [13 x i8] c"b equal zero\00"
@s2 = private constant [17 x i8] c"now b is equal 2\00"


%list = type {
i32,
%list*
}
@list_size = constant i32 ptrtoint (%list* getelementptr (%list, %list* null, i32 1) to i32)


define i32 @main() {
L0:
	%r0 = alloca %list*
	store %list* null, %list** %r0
	%r1 = call i8* @malloc(i32 2)
	%r2 = bitcast i8* %r1 to %list*
	%r3 = alloca %list*
	store %list* %r2, %list** %r3
	%r4 = load %list*, %list** %r3
	%r5 = getelementptr %list, %list* %r4, i32 0, i32 0
	%r6 = load i32, i32* %r5
	%r7 = icmp eq i32 %r6, 0
	br i1 %r7, label %L1, label %L2
L1:
	%r8 = bitcast [13 x i8]* @s1 to i8*
	call void @printString(i8* %r8)
	br label %L2
L2:
	%r9 = load %list*, %list** %r3
	%r10 = getelementptr %list, %list* %r9, i32 0, i32 0
	store i32 2, i32* %r10
	%r11 = load %list*, %list** %r3
	%r12 = getelementptr %list, %list* %r11, i32 0, i32 0
	%r13 = load i32, i32* %r12
	%r14 = icmp eq i32 %r13, 2
	br i1 %r14, label %L3, label %L4
L3:
	%r15 = bitcast [17 x i8]* @s2 to i8*
	call void @printString(i8* %r15)
	br label %L4
L4:
	ret i32 0
}

