declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)




%CallableNested = type {
	i32
}

%fCallable = type {
	i32,
	%CallableNested*
}

%havingCallable = type {
	%fCallable*
}



define void @CallableNested__setZ(%CallableNested* %self, i32 %z) {
L0:
	%r0 = alloca %CallableNested*
	store %CallableNested* %self, %CallableNested** %r0
	%r1 = alloca i32
	store i32 %z, i32* %r1
	%r2 = load %CallableNested*, %CallableNested** %r0
	%r3 = getelementptr %CallableNested, %CallableNested* %r2, i32 0, i32 0
	%r4 = load i32, i32* %r1
	store i32 %r4, i32* %r3
	ret void
}

define void @CallableNested__printZ(%CallableNested* %self) {
L0:
	%r0 = alloca %CallableNested*
	store %CallableNested* %self, %CallableNested** %r0
	%r1 = load %CallableNested*, %CallableNested** %r0
	%r2 = getelementptr %CallableNested, %CallableNested* %r1, i32 0, i32 0
	%r3 = load i32, i32* %r2
	call void @printInt(i32 %r3)
	ret void
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
	%r6 = call i8* @malloc(i32 12)
	%r7 = bitcast i8* %r6 to %fCallable*
	%r8 = getelementptr %fCallable, %fCallable* %r7, i32 0, i32 0
	store i32 0, i32* %r8
	%r9 = getelementptr %fCallable, %fCallable* %r7, i32 0, i32 1
	store %CallableNested* null, %CallableNested** %r9
	store %fCallable* %r7, %fCallable** %r5
	%r10 = load %havingCallable*, %havingCallable** %r0
	%r11 = getelementptr %havingCallable, %havingCallable* %r10, i32 0, i32 0
	%r12 = load %fCallable*, %fCallable** %r11
	%r13 = getelementptr %fCallable, %fCallable* %r12, i32 0, i32 1
	%r14 = call i8* @malloc(i32 4)
	%r15 = bitcast i8* %r14 to %CallableNested*
	%r16 = getelementptr %CallableNested, %CallableNested* %r15, i32 0, i32 0
	store i32 0, i32* %r16
	store %CallableNested* %r15, %CallableNested** %r13
	%r17 = load %havingCallable*, %havingCallable** %r0
	%r18 = getelementptr %havingCallable, %havingCallable* %r17, i32 0, i32 0
	%r19 = load %fCallable*, %fCallable** %r18
	%r20 = getelementptr %fCallable, %fCallable* %r19, i32 0, i32 0
	store i32 13, i32* %r20
	%r21 = load %havingCallable*, %havingCallable** %r0
	%r22 = bitcast %havingCallable* %r21 to %havingCallable*
	call void @havingCallable__test(%havingCallable* %r22)
	%r23 = load %havingCallable*, %havingCallable** %r0
	%r24 = getelementptr %havingCallable, %havingCallable* %r23, i32 0, i32 0
	%r25 = load %fCallable*, %fCallable** %r24
	%r26 = getelementptr %fCallable, %fCallable* %r25, i32 0, i32 0
	store i32 42, i32* %r26
	%r27 = load %havingCallable*, %havingCallable** %r0
	%r28 = getelementptr %havingCallable, %havingCallable* %r27, i32 0, i32 0
	%r29 = load %fCallable*, %fCallable** %r28
	%r30 = bitcast %fCallable* %r29 to %fCallable*
	call void @fCallable__printX(%fCallable* %r30)
	%r31 = load %havingCallable*, %havingCallable** %r0
	%r32 = getelementptr %havingCallable, %havingCallable* %r31, i32 0, i32 0
	%r33 = load %fCallable*, %fCallable** %r32
	%r34 = getelementptr %fCallable, %fCallable* %r33, i32 0, i32 1
	%r35 = load %CallableNested*, %CallableNested** %r34
	%r36 = bitcast %CallableNested* %r35 to %CallableNested*
	call void @CallableNested__setZ(%CallableNested* %r36,i32 100)
	%r37 = load %havingCallable*, %havingCallable** %r0
	%r38 = getelementptr %havingCallable, %havingCallable* %r37, i32 0, i32 0
	%r39 = load %fCallable*, %fCallable** %r38
	%r40 = getelementptr %fCallable, %fCallable* %r39, i32 0, i32 1
	%r41 = load %CallableNested*, %CallableNested** %r40
	%r42 = bitcast %CallableNested* %r41 to %CallableNested*
	call void @CallableNested__printZ(%CallableNested* %r42)
	ret i32 0
}

