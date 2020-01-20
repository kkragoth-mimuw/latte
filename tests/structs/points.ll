declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)




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
	%r0 = alloca %Point2*
	store %Point2* null, %Point2** %r0
	%r1 = alloca %Point3*
	store %Point3* null, %Point3** %r1
	%r2 = alloca %Point4*
	store %Point4* null, %Point4** %r2
	%r3 = call i8* @malloc(i32 12)
	%r4 = bitcast i8* %r3 to %Point3*
	%r5 = getelementptr %Point3, %Point3* %r4, i32 0, i32 0
	store i32 0, i32* %r5
	%r6 = getelementptr %Point3, %Point3* %r4, i32 0, i32 1
	store i32 0, i32* %r6
	%r7 = getelementptr %Point3, %Point3* %r4, i32 0, i32 2
	store i32 0, i32* %r7
	%r8 = bitcast %Point3* %r4 to %Point2*
	store %Point2* %r8, %Point2** %r0
	%r9 = call i8* @malloc(i32 12)
	%r10 = bitcast i8* %r9 to %Point3*
	%r11 = getelementptr %Point3, %Point3* %r10, i32 0, i32 0
	store i32 0, i32* %r11
	%r12 = getelementptr %Point3, %Point3* %r10, i32 0, i32 1
	store i32 0, i32* %r12
	%r13 = getelementptr %Point3, %Point3* %r10, i32 0, i32 2
	store i32 0, i32* %r13
	store %Point3* %r10, %Point3** %r1
	%r14 = call i8* @malloc(i32 16)
	%r15 = bitcast i8* %r14 to %Point4*
	%r16 = getelementptr %Point4, %Point4* %r15, i32 0, i32 0
	store i32 0, i32* %r16
	%r17 = getelementptr %Point4, %Point4* %r15, i32 0, i32 1
	store i32 0, i32* %r17
	%r18 = getelementptr %Point4, %Point4* %r15, i32 0, i32 2
	store i32 0, i32* %r18
	%r19 = getelementptr %Point4, %Point4* %r15, i32 0, i32 3
	store i32 0, i32* %r19
	store %Point4* %r15, %Point4** %r2
	%r20 = load %Point3*, %Point3** %r1
	%r21 = bitcast %Point3* %r20 to %Point2*
	call void @Point2__move(%Point2* %r21,i32 2,i32 4)
	%r22 = load %Point3*, %Point3** %r1
	%r23 = bitcast %Point3* %r22 to %Point3*
	call void @Point3__moveZ(%Point3* %r23,i32 7)
	%r24 = load %Point3*, %Point3** %r1
	%r25 = bitcast %Point3* %r24 to %Point2*
	store %Point2* %r25, %Point2** %r0
	%r26 = load %Point2*, %Point2** %r0
	%r27 = bitcast %Point2* %r26 to %Point2*
	call void @Point2__move(%Point2* %r27,i32 3,i32 5)
	%r28 = load %Point4*, %Point4** %r2
	%r29 = bitcast %Point4* %r28 to %Point2*
	call void @Point2__move(%Point2* %r29,i32 1,i32 3)
	%r30 = load %Point4*, %Point4** %r2
	%r31 = bitcast %Point4* %r30 to %Point3*
	call void @Point3__moveZ(%Point3* %r31,i32 6)
	%r32 = load %Point4*, %Point4** %r2
	%r33 = bitcast %Point4* %r32 to %Point4*
	call void @Point4__moveW(%Point4* %r33,i32 2)
	%r34 = load %Point2*, %Point2** %r0
	%r35 = bitcast %Point2* %r34 to %Point2*
	%r36 = call i32 @Point2__getX(%Point2* %r35)
	call void @printInt(i32 %r36)
	%r37 = load %Point2*, %Point2** %r0
	%r38 = bitcast %Point2* %r37 to %Point2*
	%r39 = call i32 @Point2__getY(%Point2* %r38)
	call void @printInt(i32 %r39)
	%r40 = load %Point3*, %Point3** %r1
	%r41 = bitcast %Point3* %r40 to %Point3*
	%r42 = call i32 @Point3__getZ(%Point3* %r41)
	call void @printInt(i32 %r42)
	%r43 = load %Point4*, %Point4** %r2
	%r44 = bitcast %Point4* %r43 to %Point4*
	%r45 = call i32 @Point4__getW(%Point4* %r44)
	call void @printInt(i32 %r45)
	ret i32 0
}

