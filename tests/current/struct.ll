declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)




%list = type {
i32,
%list*
}
@list_size = constant i32 ptrtoint (%list* getelementptr (%list, %list* null, i32 1) to i32)


define i32 @main() {
L0:
	%r0 = call %list* @fromTo(i32 1,i32 50)
	%r1 = call i32 @length(%list* %r0)
	call void @printInt(i32 %r1)
	%r2 = call %list* @fromTo(i32 1,i32 100)
	%r3 = call i32 @length2(%list* %r2)
	call void @printInt(i32 %r3)
	ret i32 0
}

define i32 @head(%list* %xs) {
L0:
	%r0 = alloca %list*
	store %list* %xs, %list** %r0
	%r1 = load %list*, %list** %r0
	%r2 = getelementptr %list, %list* %r1, i32 0, i32 0
	%r3 = load i32, i32* %r2
	ret i32 %r3
}

define %list* @cons(i32 %x, %list* %xs) {
L0:
	%r0 = alloca i32
	store i32 %x, i32* %r0
	%r1 = alloca %list*
	store %list* %xs, %list** %r1
	%r2 = alloca %list*
	store %list* null, %list** %r2
	%r3 = call i8* @malloc(i32 12)
	%r4 = bitcast i8* %r3 to %list*
	store %list* %r4, %list** %r2
	%r5 = load %list*, %list** %r2
	%r6 = getelementptr %list, %list* %r5, i32 0, i32 0
	%r7 = load i32, i32* %r0
	store i32 %r7, i32* %r6
	%r8 = load %list*, %list** %r2
	%r9 = getelementptr %list, %list* %r8, i32 0, i32 1
	%r10 = load %list*, %list** %r1
	store %list* %r10, %list** %r9
	%r11 = load %list*, %list** %r2
	ret %list* %r11
}

define i32 @length(%list* %xs) {
L0:
	%r0 = alloca %list*
	store %list* %xs, %list** %r0
	%r1 = load %list*, %list** %r0
	%r2 = icmp eq %list* %r1, null
	br i1 %r2, label %L1, label %L2
L1:
	ret i32 0
L2:
	%r3 = load %list*, %list** %r0
	%r4 = getelementptr %list, %list* %r3, i32 0, i32 1
	%r5 = load %list*, %list** %r4
	%r6 = call i32 @length(%list* %r5)
	%r7 = add i32 1, %r6
	ret i32 %r7
}

define %list* @fromTo(i32 %m, i32 %n) {
L0:
	%r0 = alloca i32
	store i32 %m, i32* %r0
	%r1 = alloca i32
	store i32 %n, i32* %r1
	%r2 = load i32, i32* %r0
	%r3 = load i32, i32* %r1
	%r4 = icmp sgt i32 %r2, %r3
	br i1 %r4, label %L1, label %L2
L1:
	ret %list* null
L2:
	%r5 = load i32, i32* %r0
	%r6 = load i32, i32* %r0
	%r7 = add i32 %r6, 1
	%r8 = load i32, i32* %r1
	%r9 = call %list* @fromTo(i32 %r7,i32 %r8)
	%r10 = call %list* @cons(i32 %r5,%list* %r9)
	ret %list* %r10
}

define i32 @length2(%list* %xs) {
L0:
	%r0 = alloca %list*
	store %list* %xs, %list** %r0
	%r1 = alloca i32
	store i32 0, i32* %r1
	br label %L1
L1:
	%r2 = load %list*, %list** %r0
	%r3 = icmp ne %list* %r2, null
	br i1 %r3, label %L2, label %L3
L2:
	%r4 = load i32, i32* %r1
	%r5 = add i32 %r4, 1
	store i32 %r5, i32* %r1
	%r6 = load %list*, %list** %r0
	%r7 = getelementptr %list, %list* %r6, i32 0, i32 1
	%r8 = load %list*, %list** %r7
	store %list* %r8, %list** %r0
	br label %L1
L3:
	%r9 = load i32, i32* %r1
	ret i32 %r9
}

