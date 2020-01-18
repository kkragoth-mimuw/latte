declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)

@s1 = private constant [10 x i8] c"a is null\00"
@s2 = private constant [10 x i8] c"b is null\00"


%list = type {
i32,
%list*
}

define i32 @main() {
L0:
	%r0 = alloca %list*
	store %list* null, %list** %r0
	%r1 = call i8* @malloc(i32 2)
	%r2 = bitcast i8* %r1 to %list*
	%r3 = alloca %list*
	store %list* %r2, %list** %r3
	%r4 = load %list*, %list** %r0
	%r5 = icmp eq %list* %r4, null
	br i1 %r5, label %L1, label %L2
L1:
	%r6 = bitcast [10 x i8]* @s1 to i8*
	call void @printString(i8* %r6)
	br label %L2
L2:
	%r7 = load %list*, %list** %r3
	%r8 = icmp eq %list* %r7, null
	br i1 %r8, label %L3, label %L4
L3:
	%r9 = bitcast [10 x i8]* @s2 to i8*
	call void @printString(i8* %r9)
	br label %L4
L4:
	ret i32 0
}

