declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)




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
	%r2 = call i8* @malloc(i32 12)
	%r3 = bitcast i8* %r2 to %Node*
	%r4 = getelementptr %Node, %Node* %r3, i32 0, i32 0
	store i32 0, i32* %r4
	%r5 = getelementptr %Node, %Node* %r3, i32 0, i32 1
	store %Node* null, %Node** %r5
	%r6 = alloca %Node*
	store %Node* %r3, %Node** %r6
	%r7 = load %Node*, %Node** %r6
	%r8 = load i32, i32* %r1
	call void @Node__setElem(%Node* %r7,i32 %r8)
	%r9 = load %Node*, %Node** %r6
	%r10 = load %Stack*, %Stack** %r0
	%r11 = getelementptr %Stack, %Stack* %r10, i32 0, i32 0
	%r12 = load %Node*, %Node** %r11
	call void @Node__setNext(%Node* %r9,%Node* %r12)
	%r13 = load %Stack*, %Stack** %r0
	%r14 = getelementptr %Stack, %Stack* %r13, i32 0, i32 0
	%r15 = load %Node*, %Node** %r6
	store %Node* %r15, %Node** %r14
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
	%r4 = call i32 @Node__getElem(%Node* %r3)
	ret i32 %r4
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
	%r6 = call %Node* @Node__getNext(%Node* %r5)
	store %Node* %r6, %Node** %r2
	ret void
}


define i32 @main() {
L0:
	%r0 = call i8* @malloc(i32 8)
	%r1 = bitcast i8* %r0 to %Stack*
	%r2 = getelementptr %Stack, %Stack* %r1, i32 0, i32 0
	store %Node* null, %Node** %r2
	%r3 = alloca %Stack*
	store %Stack* %r1, %Stack** %r3
	%r4 = alloca i32
	store i32 0, i32* %r4
	br label %L1
L1:
	%r5 = load i32, i32* %r4
	%r6 = icmp slt i32 %r5, 10
	br i1 %r6, label %L2, label %L3
L2:
	%r7 = load %Stack*, %Stack** %r3
	%r8 = load i32, i32* %r4
	call void @Stack__push(%Stack* %r7,i32 %r8)
	%r9 = load i32, i32* %r4
	%r10 = add i32 %r9, 1
	store i32 %r10, i32* %r4
	br label %L1
L3:
	br label %L4
L4:
	%r11 = load %Stack*, %Stack** %r3
	%r12 = call i1 @Stack__isEmpty(%Stack* %r11)
	%r13 = xor i1 1, %r12
	br i1 %r13, label %L5, label %L6
L5:
	%r14 = load %Stack*, %Stack** %r3
	%r15 = call i32 @Stack__top(%Stack* %r14)
	call void @printInt(i32 %r15)
	%r16 = load %Stack*, %Stack** %r3
	call void @Stack__pop(%Stack* %r16)
	br label %L4
L6:
	ret i32 0
}

