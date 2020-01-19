declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)




%Counter = type {
	i32
}



define void @Counter__incr(%Counter* %self) {
L0:
	%r0 = alloca %Counter*
	store %Counter* %self, %Counter** %r0
	%r1 = load %Counter*, %Counter** %r0
	%r2 = getelementptr %Counter, %Counter* %r1, i32 0, i32 0
	%r3 = load %Counter*, %Counter** %r0
	%r4 = getelementptr %Counter, %Counter* %r3, i32 0, i32 0
	%r5 = load i32, i32* %r4
	%r6 = add i32 %r5, 1
	store i32 %r6, i32* %r2
	ret void
}

define i32 @Counter__value(%Counter* %self) {
L0:
	%r0 = alloca %Counter*
	store %Counter* %self, %Counter** %r0
	%r1 = load %Counter*, %Counter** %r0
	%r2 = getelementptr %Counter, %Counter* %r1, i32 0, i32 0
	%r3 = load i32, i32* %r2
	ret i32 %r3
}


define i32 @main() {
L0:
	%r0 = alloca %Counter*
	store %Counter* null, %Counter** %r0
	%r1 = call i8* @malloc(i32 4)
	%r2 = bitcast i8* %r1 to %Counter*
	%r3 = getelementptr %Counter, %Counter* %r2, i32 0, i32 0
	store i32 0, i32* %r3
	store %Counter* %r2, %Counter** %r0
	%r4 = load %Counter*, %Counter** %r0
	call void @Counter__incr(%Counter* %r4)
	%r5 = load %Counter*, %Counter** %r0
	call void @Counter__incr(%Counter* %r5)
	%r6 = load %Counter*, %Counter** %r0
	call void @Counter__incr(%Counter* %r6)
	%r7 = load %Counter*, %Counter** %r0
	%r8 = call i32 @Counter__value(%Counter* %r7)
	%r9 = alloca i32
	store i32 %r8, i32* %r9
	%r10 = load i32, i32* %r9
	call void @printInt(i32 %r10)
	ret i32 0
}

