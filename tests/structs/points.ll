declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)




%Point2 = type {
	i32,
	i32
}

%Point3 = type {
	i32,
	i32,
	i32
}

%Point4 = type {
	i32,
	i32,
	i32,
	i32
}



define void @Point2__move(%Point2* %self, i32 %dx, i32 %dy) {
L0:
	%r0 = alloca %Point2*
	store %Point2* %self, %Point2** %r0
	%r1 = alloca i32
	store i32 %dx, i32* %r1
	%r2 = alloca i32
	store i32 %dy, i32* %r2
	%r3 = load %Point2*, %Point2** %r0
	%r4 = getelementptr %Point2, %Point2* %r3, i32 0, i32 0
	%r5 = load %Point2*, %Point2** %r0
	%r6 = getelementptr %Point2, %Point2* %r5, i32 0, i32 0
	%r7 = load i32, i32* %r6
	%r8 = load i32, i32* %r1
	%r9 = add i32 %r7, %r8
	store i32 %r9, i32* %r4
	%r10 = load %Point2*, %Point2** %r0
	%r11 = getelementptr %Point2, %Point2* %r10, i32 0, i32 1
	%r12 = load %Point2*, %Point2** %r0
	%r13 = getelementptr %Point2, %Point2* %r12, i32 0, i32 1
	%r14 = load i32, i32* %r13
	%r15 = load i32, i32* %r2
	%r16 = add i32 %r14, %r15
	store i32 %r16, i32* %r11
	ret void
}

define i32 @Point2__getX(%Point2* %self) {
L0:
	%r0 = alloca %Point2*
	store %Point2* %self, %Point2** %r0
	%r1 = load %Point2*, %Point2** %r0
	%r2 = getelementptr %Point2, %Point2* %r1, i32 0, i32 0
	%r3 = load i32, i32* %r2
	ret i32 %r3
}

define i32 @Point2__getY(%Point2* %self) {
L0:
	%r0 = alloca %Point2*
	store %Point2* %self, %Point2** %r0
	%r1 = load %Point2*, %Point2** %r0
	%r2 = getelementptr %Point2, %Point2* %r1, i32 0, i32 1
	%r3 = load i32, i32* %r2
	ret i32 %r3
}


define void @Point3__move(%Point3* %self, i32 %dx, i32 %dy) {
L0:
	%r0 = alloca %Point3*
	store %Point3* %self, %Point3** %r0
	%r1 = alloca i32
	store i32 %dx, i32* %r1
	%r2 = alloca i32
	store i32 %dy, i32* %r2
	%r3 = load %Point3*, %Point3** %r0
	%r4 = getelementptr %Point3, %Point3* %r3, i32 0, i32 0
	%r5 = load %Point3*, %Point3** %r0
	%r6 = getelementptr %Point3, %Point3* %r5, i32 0, i32 0
	%r7 = load i32, i32* %r6
	%r8 = load i32, i32* %r1
	%r9 = add i32 %r7, %r8
	store i32 %r9, i32* %r4
	%r10 = load %Point3*, %Point3** %r0
	%r11 = getelementptr %Point3, %Point3* %r10, i32 0, i32 1
	%r12 = load %Point3*, %Point3** %r0
	%r13 = getelementptr %Point3, %Point3* %r12, i32 0, i32 1
	%r14 = load i32, i32* %r13
	%r15 = load i32, i32* %r2
	%r16 = add i32 %r14, %r15
	store i32 %r16, i32* %r11
	ret void
}

define i32 @Point3__getX(%Point3* %self) {
L0:
	%r0 = alloca %Point3*
	store %Point3* %self, %Point3** %r0
	%r1 = load %Point3*, %Point3** %r0
	%r2 = getelementptr %Point3, %Point3* %r1, i32 0, i32 0
	%r3 = load i32, i32* %r2
	ret i32 %r3
}

define i32 @Point3__getY(%Point3* %self) {
L0:
	%r0 = alloca %Point3*
	store %Point3* %self, %Point3** %r0
	%r1 = load %Point3*, %Point3** %r0
	%r2 = getelementptr %Point3, %Point3* %r1, i32 0, i32 1
	%r3 = load i32, i32* %r2
	ret i32 %r3
}

define void @Point3__moveZ(%Point3* %self, i32 %dz) {
L0:
	%r0 = alloca %Point3*
	store %Point3* %self, %Point3** %r0
	%r1 = alloca i32
	store i32 %dz, i32* %r1
	%r2 = load %Point3*, %Point3** %r0
	%r3 = getelementptr %Point3, %Point3* %r2, i32 0, i32 2
	%r4 = load %Point3*, %Point3** %r0
	%r5 = getelementptr %Point3, %Point3* %r4, i32 0, i32 2
	%r6 = load i32, i32* %r5
	%r7 = load i32, i32* %r1
	%r8 = add i32 %r6, %r7
	store i32 %r8, i32* %r3
	ret void
}

define i32 @Point3__getZ(%Point3* %self) {
L0:
	%r0 = alloca %Point3*
	store %Point3* %self, %Point3** %r0
	%r1 = load %Point3*, %Point3** %r0
	%r2 = getelementptr %Point3, %Point3* %r1, i32 0, i32 2
	%r3 = load i32, i32* %r2
	ret i32 %r3
}


define void @Point4__move(%Point4* %self, i32 %dx, i32 %dy) {
L0:
	%r0 = alloca %Point4*
	store %Point4* %self, %Point4** %r0
	%r1 = alloca i32
	store i32 %dx, i32* %r1
	%r2 = alloca i32
	store i32 %dy, i32* %r2
	%r3 = load %Point4*, %Point4** %r0
	%r4 = getelementptr %Point4, %Point4* %r3, i32 0, i32 0
	%r5 = load %Point4*, %Point4** %r0
	%r6 = getelementptr %Point4, %Point4* %r5, i32 0, i32 0
	%r7 = load i32, i32* %r6
	%r8 = load i32, i32* %r1
	%r9 = add i32 %r7, %r8
	store i32 %r9, i32* %r4
	%r10 = load %Point4*, %Point4** %r0
	%r11 = getelementptr %Point4, %Point4* %r10, i32 0, i32 1
	%r12 = load %Point4*, %Point4** %r0
	%r13 = getelementptr %Point4, %Point4* %r12, i32 0, i32 1
	%r14 = load i32, i32* %r13
	%r15 = load i32, i32* %r2
	%r16 = add i32 %r14, %r15
	store i32 %r16, i32* %r11
	ret void
}

define i32 @Point4__getX(%Point4* %self) {
L0:
	%r0 = alloca %Point4*
	store %Point4* %self, %Point4** %r0
	%r1 = load %Point4*, %Point4** %r0
	%r2 = getelementptr %Point4, %Point4* %r1, i32 0, i32 0
	%r3 = load i32, i32* %r2
	ret i32 %r3
}

define i32 @Point4__getY(%Point4* %self) {
L0:
	%r0 = alloca %Point4*
	store %Point4* %self, %Point4** %r0
	%r1 = load %Point4*, %Point4** %r0
	%r2 = getelementptr %Point4, %Point4* %r1, i32 0, i32 1
	%r3 = load i32, i32* %r2
	ret i32 %r3
}

define void @Point4__moveZ(%Point4* %self, i32 %dz) {
L0:
	%r0 = alloca %Point4*
	store %Point4* %self, %Point4** %r0
	%r1 = alloca i32
	store i32 %dz, i32* %r1
	%r2 = load %Point4*, %Point4** %r0
	%r3 = getelementptr %Point4, %Point4* %r2, i32 0, i32 2
	%r4 = load %Point4*, %Point4** %r0
	%r5 = getelementptr %Point4, %Point4* %r4, i32 0, i32 2
	%r6 = load i32, i32* %r5
	%r7 = load i32, i32* %r1
	%r8 = add i32 %r6, %r7
	store i32 %r8, i32* %r3
	ret void
}

define i32 @Point4__getZ(%Point4* %self) {
L0:
	%r0 = alloca %Point4*
	store %Point4* %self, %Point4** %r0
	%r1 = load %Point4*, %Point4** %r0
	%r2 = getelementptr %Point4, %Point4* %r1, i32 0, i32 2
	%r3 = load i32, i32* %r2
	ret i32 %r3
}

define void @Point4__moveW(%Point4* %self, i32 %dw) {
L0:
	%r0 = alloca %Point4*
	store %Point4* %self, %Point4** %r0
	%r1 = alloca i32
	store i32 %dw, i32* %r1
	%r2 = load %Point4*, %Point4** %r0
	%r3 = getelementptr %Point4, %Point4* %r2, i32 0, i32 3
	%r4 = load %Point4*, %Point4** %r0
	%r5 = getelementptr %Point4, %Point4* %r4, i32 0, i32 3
	%r6 = load i32, i32* %r5
	%r7 = load i32, i32* %r1
	%r8 = add i32 %r6, %r7
	store i32 %r8, i32* %r3
	ret void
}

define i32 @Point4__getW(%Point4* %self) {
L0:
	%r0 = alloca %Point4*
	store %Point4* %self, %Point4** %r0
	%r1 = load %Point4*, %Point4** %r0
	%r2 = getelementptr %Point4, %Point4* %r1, i32 0, i32 3
	%r3 = load i32, i32* %r2
	ret i32 %r3
}


define i32 @main() {
L0:
	%r0 = call i8* @malloc(i32 12)
	%r1 = bitcast i8* %r0 to %Point3*
	%r2 = getelementptr %Point3, %Point3* %r1, i32 0, i32 0
	store i32 0, i32* %r2
	%r3 = getelementptr %Point3, %Point3* %r1, i32 0, i32 1
	store i32 0, i32* %r3
	%r4 = getelementptr %Point3, %Point3* %r1, i32 0, i32 2
	store i32 0, i32* %r4
	%r5 = alloca %Point2*
	store %Point3* %r1, %Point2** %r5
	%r6 = call i8* @malloc(i32 12)
	%r7 = bitcast i8* %r6 to %Point3*
	%r8 = getelementptr %Point3, %Point3* %r7, i32 0, i32 0
	store i32 0, i32* %r8
	%r9 = getelementptr %Point3, %Point3* %r7, i32 0, i32 1
	store i32 0, i32* %r9
	%r10 = getelementptr %Point3, %Point3* %r7, i32 0, i32 2
	store i32 0, i32* %r10
	%r11 = alloca %Point3*
	store %Point3* %r7, %Point3** %r11
	%r12 = call i8* @malloc(i32 16)
	%r13 = bitcast i8* %r12 to %Point4*
	%r14 = getelementptr %Point4, %Point4* %r13, i32 0, i32 0
	store i32 0, i32* %r14
	%r15 = getelementptr %Point4, %Point4* %r13, i32 0, i32 1
	store i32 0, i32* %r15
	%r16 = getelementptr %Point4, %Point4* %r13, i32 0, i32 2
	store i32 0, i32* %r16
	%r17 = getelementptr %Point4, %Point4* %r13, i32 0, i32 3
	store i32 0, i32* %r17
	%r18 = alloca %Point4*
	store %Point4* %r13, %Point4** %r18
	%r19 = load %Point3*, %Point3** %r11
	call void @Point3__move(%Point3* %r19,i32 2,i32 4)
	%r20 = load %Point3*, %Point3** %r11
	call void @Point3__moveZ(%Point3* %r20,i32 7)
	%r21 = load %Point3*, %Point3** %r11
	%r22 = bitcast %Point3* %r21 to %Point2*
	store %Point2* %r22, %Point2** %r5
	%r23 = load %Point2*, %Point2** %r5
	call void @Point2__move(%Point2* %r23,i32 3,i32 5)
	%r24 = load %Point4*, %Point4** %r18
	call void @Point4__move(%Point4* %r24,i32 1,i32 3)
	%r25 = load %Point4*, %Point4** %r18
	call void @Point4__moveZ(%Point4* %r25,i32 6)
	%r26 = load %Point4*, %Point4** %r18
	call void @Point4__moveW(%Point4* %r26,i32 2)
	%r27 = load %Point2*, %Point2** %r5
	%r28 = call i32 @Point2__getX(%Point2* %r27)
	call void @printInt(i32 %r28)
	%r29 = load %Point2*, %Point2** %r5
	%r30 = call i32 @Point2__getY(%Point2* %r29)
	call void @printInt(i32 %r30)
	%r31 = load %Point3*, %Point3** %r11
	%r32 = call i32 @Point3__getZ(%Point3* %r31)
	call void @printInt(i32 %r32)
	%r33 = load %Point4*, %Point4** %r18
	%r34 = call i32 @Point4__getW(%Point4* %r33)
	call void @printInt(i32 %r34)
	ret i32 0
}

