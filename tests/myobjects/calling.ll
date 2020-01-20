declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)




%fCallable = type {
	i32
}

%havingCallable = type {
	%fCallable*
}



define void @fCallable__printX(%fCallable* %self) {
L0:
	%r0 = alloca %fCallable*
	store %fCallable* %self, %fCallable** %r0
	%r1 = load %fCallable*, %fCallable** %r0
	%r2 = getelementptr %fCallable, %fCallable* %r1, i32 0, i32 0
	%r3 = load i32, i32* %r2
	call void @printInt(i32 %r3)
	ret void
}


define void @havingCallable__test(%havingCallable* %self) {
L0:
	%r0 = alloca %havingCallable*
	store %havingCallable* %self, %havingCallable** %r0
	%r1 = load %havingCallable*, %havingCallable** %r0
	%r2 = getelementptr %havingCallable, %havingCallable* %r1, i32 0, i32 0
	%r3 = load %fCallable*, %fCallable** %r2
	%r4 = bitcast %fCallable* %r3 to %fCallable*
	call void @fCallable__printX(%fCallable* %r4)
	ret void
}


define i32 @main() {
L0:
	%r0 = alloca %havingCallable*
	store %havingCallable* null, %havingCallable** %r0
	%r1 = call i8* @malloc(i32 8)
	%r2 = bitcast i8* %r1 to %havingCallable*
	%r3 = getelementptr %havingCallable, %havingCallable* %r2, i32 0, i32 0
	store %fCallable* null, %fCallable** %r3
	store %havingCallable* %r2, %havingCallable** %r0
	%r4 = load %havingCallable*, %havingCallable** %r0
	%r5 = getelementptr %havingCallable, %havingCallable* %r4, i32 0, i32 0
	%r6 = call i8* @malloc(i32 4)
	%r7 = bitcast i8* %r6 to %fCallable*
	%r8 = getelementptr %fCallable, %fCallable* %r7, i32 0, i32 0
	store i32 0, i32* %r8
	store %fCallable* %r7, %fCallable** %r5
	%r9 = load %havingCallable*, %havingCallable** %r0
	%r10 = getelementptr %havingCallable, %havingCallable* %r9, i32 0, i32 0
	%r11 = load %fCallable*, %fCallable** %r10
	%r12 = getelementptr %fCallable, %fCallable* %r11, i32 0, i32 0
	store i32 13, i32* %r12
	%r13 = load %havingCallable*, %havingCallable** %r0
	%r14 = bitcast %havingCallable* %r13 to %havingCallable*
	call void @havingCallable__test(%havingCallable* %r14)
	%r15 = load %havingCallable*, %havingCallable** %r0
	%r16 = getelementptr %havingCallable, %havingCallable* %r15, i32 0, i32 0
	%r17 = load %fCallable*, %fCallable** %r16
	%r18 = getelementptr %fCallable, %fCallable* %r17, i32 0, i32 0
	store i32 42, i32* %r18
	%r19 = load %havingCallable*, %havingCallable** %r0
	%r20 = getelementptr %havingCallable, %havingCallable* %r19, i32 0, i32 0
	%r21 = load %fCallable*, %fCallable** %r20
	%r22 = bitcast %fCallable* %r21 to %fCallable*
	call void @fCallable__printX(%fCallable* %r22)
	ret i32 0
}

