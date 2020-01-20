declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)

@s1 = private constant [18 x i8] c"SHOULD BE PRINTED\00"
@s2 = private constant [21 x i8] c"shouldnt be printed!\00"


%list = type {
	i32,
	%list*
}



define void @list__printX(%list* %self) {
L0:
	%r0 = alloca %list*
	store %list* %self, %list** %r0
	%r1 = bitcast [18 x i8]* @s1 to i8*
	call void @printString(i8* %r1)
	ret void
}

define void @list__test(%list* %self) {
L0:
	%r0 = alloca %list*
	store %list* %self, %list** %r0
	%r1 = load %list*, %list** %r0
	%r2 = bitcast %list* %r1 to %list*
	call void @list__printX(%list* %r2)
	ret void
}


define void @printX() {
L0:
	%r0 = bitcast [21 x i8]* @s2 to i8*
	call void @printString(i8* %r0)
	ret void
}

define i32 @main() {
L0:
	%r0 = alloca %list*
	store %list* null, %list** %r0
	%r1 = call i8* @malloc(i32 12)
	%r2 = bitcast i8* %r1 to %list*
	%r3 = getelementptr %list, %list* %r2, i32 0, i32 0
	store i32 0, i32* %r3
	%r4 = getelementptr %list, %list* %r2, i32 0, i32 1
	store %list* null, %list** %r4
	store %list* %r2, %list** %r0
	%r5 = load %list*, %list** %r0
	%r6 = bitcast %list* %r5 to %list*
	call void @list__test(%list* %r6)
	ret i32 0
}

