declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)




%Node = type {
	i32,
	%Node*
}

%Stack = type {
	%Node*
}



define void @Node__setElem(%Node* %self, i32 %c) {
L0:
	%r0 = alloca %Node*
	store %Node* %self, %Node** %r0
	%r1 = alloca i32
	store i32 %c, i32* %r1
	%r2 = load %Node*, %Node** %r0
	%r3 = getelementptr %Node, %Node* %r2, i32 0, i32 0
	%r4 = load i32, i32* %r1
	store i32 %r4, i32* %r3
	ret void
}

define void @Node__setNext(%Node* %self, %Node* %n) {
L0:
	%r0 = alloca %Node*
	store %Node* %self, %Node** %r0
	%r1 = alloca %Node*
	store %Node* %n, %Node** %r1
	%r2 = load %Node*, %Node** %r0
	%r3 = getelementptr %Node, %Node* %r2, i32 0, i32 1
	%r4 = load %Node*, %Node** %r1
	store %Node* %r4, %Node** %r3
	ret void
}

define i32 @Node__getElem(%Node* %self) {
L0:
	%r0 = alloca %Node*
	store %Node* %self, %Node** %r0
	%r1 = load %Node*, %Node** %r0
	%r2 = getelementptr %Node, %Node* %r1, i32 0, i32 0
	%r3 = load i32, i32* %r2
	ret i32 %r3
}

define %Node* @Node__getNext(%Node* %self) {
L0:
	%r0 = alloca %Node*
	store %Node* %self, %Node** %r0
	%r1 = load %Node*, %Node** %r0
	%r2 = getelementptr %Node, %Node* %r1, i32 0, i32 1
	%r3 = load %Node*, %Node** %r2
	ret %Node* %r3
}


define void @Stack__push(%Stack* %self, i32 %c) {
L0:
	%r0 = alloca %Stack*
	store %Stack* %self, %Stack** %r0
	%r1 = alloca i32
	store i32 %c, i32* %r1
	%r2 = alloca %Node*
	store %Node* null, %Node** %r2
	%r3 = call i8* @malloc(i32 12)
	%r4 = bitcast i8* %r3 to %Node*
	%r5 = getelementptr %Node, %Node* %r4, i32 0, i32 0
	store i32 0, i32* %r5
	%r6 = getelementptr %Node, %Node* %r4, i32 0, i32 1
	store %Node* null, %Node** %r6
	store %Node* %r4, %Node** %r2
	%r7 = load %Node*, %Node** %r2
	%r8 = load i32, i32* %r1
	%r9 = bitcast %Node* %r7 to %Node*
	call void @Node__setElem(%Node* %r9,i32 %r8)
	%r10 = load %Node*, %Node** %r2
	%r11 = load %Stack*, %Stack** %r0
	%r12 = getelementptr %Stack, %Stack* %r11, i32 0, i32 0
	%r13 = load %Node*, %Node** %r12
	%r14 = bitcast %Node* %r10 to %Node*
	%r15 = bitcast %Node* %r13 to %Node*
	call void @Node__setNext(%Node* %r14,%Node* %r15)
	%r16 = load %Stack*, %Stack** %r0
	%r17 = getelementptr %Stack, %Stack* %r16, i32 0, i32 0
	%r18 = load %Node*, %Node** %r2
	store %Node* %r18, %Node** %r17
	ret void
}

define i1 @Stack__isEmpty(%Stack* %self) {
L0:
	%r0 = alloca %Stack*
	store %Stack* %self, %Stack** %r0
	%r1 = load %Stack*, %Stack** %r0
	%r2 = getelementptr %Stack, %Stack* %r1, i32 0, i32 0
	%r3 = load %Node*, %Node** %r2
	%r4 = icmp eq %Node* %r3, null
	ret i1 %r4
}

define i32 @Stack__top(%Stack* %self) {
L0:
	%r0 = alloca %Stack*
	store %Stack* %self, %Stack** %r0
	%r1 = load %Stack*, %Stack** %r0
	%r2 = getelementptr %Stack, %Stack* %r1, i32 0, i32 0
	%r3 = load %Node*, %Node** %r2
	%r4 = bitcast %Node* %r3 to %Node*
	%r5 = call i32 @Node__getElem(%Node* %r4)
	ret i32 %r5
}

define void @Stack__pop(%Stack* %self) {
L0:
	%r0 = alloca %Stack*
	store %Stack* %self, %Stack** %r0
	%r1 = load %Stack*, %Stack** %r0
	%r2 = getelementptr %Stack, %Stack* %r1, i32 0, i32 0
	%r3 = load %Stack*, %Stack** %r0
	%r4 = getelementptr %Stack, %Stack* %r3, i32 0, i32 0
	%r5 = load %Node*, %Node** %r4
	%r6 = bitcast %Node* %r5 to %Node*
	%r7 = call %Node* @Node__getNext(%Node* %r6)
	store %Node* %r7, %Node** %r2
	ret void
}


define i32 @main() {
L0:
	%r0 = alloca %Stack*
	store %Stack* null, %Stack** %r0
	%r1 = alloca i32
	store i32 0, i32* %r1
	%r2 = call i8* @malloc(i32 8)
	%r3 = bitcast i8* %r2 to %Stack*
	%r4 = getelementptr %Stack, %Stack* %r3, i32 0, i32 0
	store %Node* null, %Node** %r4
	store %Stack* %r3, %Stack** %r0
	store i32 0, i32* %r1
	br label %L1
L1:
	%r5 = load i32, i32* %r1
	%r6 = icmp slt i32 %r5, 10
	br i1 %r6, label %L2, label %L3
L2:
	%r7 = load %Stack*, %Stack** %r0
	%r8 = load i32, i32* %r1
	%r9 = bitcast %Stack* %r7 to %Stack*
	call void @Stack__push(%Stack* %r9,i32 %r8)
	%r10 = load i32, i32* %r1
	%r11 = add i32 %r10, 1
	store i32 %r11, i32* %r1
	br label %L1
L3:
	br label %L4
L4:
	%r12 = load %Stack*, %Stack** %r0
	%r13 = bitcast %Stack* %r12 to %Stack*
	%r14 = call i1 @Stack__isEmpty(%Stack* %r13)
	%r15 = xor i1 1, %r14
	br i1 %r15, label %L5, label %L6
L5:
	%r16 = load %Stack*, %Stack** %r0
	%r17 = bitcast %Stack* %r16 to %Stack*
	%r18 = call i32 @Stack__top(%Stack* %r17)
	call void @printInt(i32 %r18)
	%r19 = load %Stack*, %Stack** %r0
	%r20 = bitcast %Stack* %r19 to %Stack*
	call void @Stack__pop(%Stack* %r20)
	br label %L4
L6:
	ret i32 0
}

