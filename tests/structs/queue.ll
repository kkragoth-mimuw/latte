declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)
declare i1 @__compareStringsEQ(i8*, i8*)
declare i1 @__compareStringsNE(i8*, i8*)




%IntQueue = type {
	%Node*,
	%Node*
}

%Node = type {
	i32,
	%Node*
}



define i1 @IntQueue__isEmpty(%IntQueue* %self) {
L0:
	%r0 = alloca %IntQueue*
	store %IntQueue* %self, %IntQueue** %r0
	%r1 = load %IntQueue*, %IntQueue** %r0
	%r2 = getelementptr %IntQueue, %IntQueue* %r1, i32 0, i32 0
	%r3 = load %Node*, %Node** %r2
	%r4 = icmp eq %Node* %r3, null
	ret i1 %r4
}

define void @IntQueue__insert(%IntQueue* %self, i32 %x) {
L0:
	%r0 = alloca %IntQueue*
	store %IntQueue* %self, %IntQueue** %r0
	%r1 = alloca i32
	store i32 %x, i32* %r1
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
	%r10 = load %IntQueue*, %IntQueue** %r0
	%r11 = bitcast %IntQueue* %r10 to %IntQueue*
	%r12 = call i1 @IntQueue__isEmpty(%IntQueue* %r11)
	br i1 %r12, label %L1, label %L2
L1:
	%r13 = load %IntQueue*, %IntQueue** %r0
	%r14 = getelementptr %IntQueue, %IntQueue* %r13, i32 0, i32 0
	%r15 = load %Node*, %Node** %r2
	store %Node* %r15, %Node** %r14
	br label %L3
L2:
	%r16 = load %IntQueue*, %IntQueue** %r0
	%r17 = getelementptr %IntQueue, %IntQueue* %r16, i32 0, i32 1
	%r18 = load %Node*, %Node** %r17
	%r19 = load %Node*, %Node** %r2
	%r20 = bitcast %Node* %r18 to %Node*
	%r21 = bitcast %Node* %r19 to %Node*
	call void @Node__setNext(%Node* %r20,%Node* %r21)
	br label %L3
L3:
	%r22 = load %IntQueue*, %IntQueue** %r0
	%r23 = getelementptr %IntQueue, %IntQueue* %r22, i32 0, i32 1
	%r24 = load %Node*, %Node** %r2
	store %Node* %r24, %Node** %r23
	ret void
}

define i32 @IntQueue__first(%IntQueue* %self) {
L0:
	%r0 = alloca %IntQueue*
	store %IntQueue* %self, %IntQueue** %r0
	%r1 = load %IntQueue*, %IntQueue** %r0
	%r2 = getelementptr %IntQueue, %IntQueue* %r1, i32 0, i32 0
	%r3 = load %Node*, %Node** %r2
	%r4 = bitcast %Node* %r3 to %Node*
	%r5 = call i32 @Node__getElem(%Node* %r4)
	ret i32 %r5
}

define void @IntQueue__rmFirst(%IntQueue* %self) {
L0:
	%r0 = alloca %IntQueue*
	store %IntQueue* %self, %IntQueue** %r0
	%r1 = load %IntQueue*, %IntQueue** %r0
	%r2 = getelementptr %IntQueue, %IntQueue* %r1, i32 0, i32 0
	%r3 = load %IntQueue*, %IntQueue** %r0
	%r4 = getelementptr %IntQueue, %IntQueue* %r3, i32 0, i32 0
	%r5 = load %Node*, %Node** %r4
	%r6 = bitcast %Node* %r5 to %Node*
	%r7 = call %Node* @Node__getNext(%Node* %r6)
	store %Node* %r7, %Node** %r2
	ret void
}

define i32 @IntQueue__size(%IntQueue* %self) {
L0:
	%r0 = alloca %IntQueue*
	store %IntQueue* %self, %IntQueue** %r0
	%r1 = alloca %Node*
	store %Node* null, %Node** %r1
	%r2 = alloca i32
	store i32 0, i32* %r2
	%r3 = load %IntQueue*, %IntQueue** %r0
	%r4 = getelementptr %IntQueue, %IntQueue* %r3, i32 0, i32 0
	%r5 = load %Node*, %Node** %r4
	store %Node* %r5, %Node** %r1
	store i32 0, i32* %r2
	br label %L1
L1:
	%r6 = load %Node*, %Node** %r1
	%r7 = icmp ne %Node* %r6, null
	br i1 %r7, label %L2, label %L3
L2:
	%r8 = load %Node*, %Node** %r1
	%r9 = bitcast %Node* %r8 to %Node*
	%r10 = call %Node* @Node__getNext(%Node* %r9)
	store %Node* %r10, %Node** %r1
	%r11 = load i32, i32* %r2
	%r12 = add i32 %r11, 1
	store i32 %r12, i32* %r2
	br label %L1
L3:
	%r13 = load i32, i32* %r2
	ret i32 %r13
}


define void @Node__setElem(%Node* %self, i32 %e) {
L0:
	%r0 = alloca %Node*
	store %Node* %self, %Node** %r0
	%r1 = alloca i32
	store i32 %e, i32* %r1
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


define i32 @f(i32 %x) {
L0:
	%r0 = alloca i32
	store i32 %x, i32* %r0
	%r1 = load i32, i32* %r0
	%r2 = load i32, i32* %r0
	%r3 = mul i32 %r1, %r2
	%r4 = add i32 %r3, 3
	ret i32 %r4
}

define i32 @main() {
L0:
	%r0 = alloca %IntQueue*
	store %IntQueue* null, %IntQueue** %r0
	%r1 = call i8* @malloc(i32 16)
	%r2 = bitcast i8* %r1 to %IntQueue*
	%r3 = getelementptr %IntQueue, %IntQueue* %r2, i32 0, i32 0
	store %Node* null, %Node** %r3
	%r4 = getelementptr %IntQueue, %IntQueue* %r2, i32 0, i32 1
	store %Node* null, %Node** %r4
	store %IntQueue* %r2, %IntQueue** %r0
	%r5 = load %IntQueue*, %IntQueue** %r0
	%r6 = call i32 @f(i32 3)
	%r7 = bitcast %IntQueue* %r5 to %IntQueue*
	call void @IntQueue__insert(%IntQueue* %r7,i32 %r6)
	%r8 = load %IntQueue*, %IntQueue** %r0
	%r9 = bitcast %IntQueue* %r8 to %IntQueue*
	call void @IntQueue__insert(%IntQueue* %r9,i32 5)
	%r10 = load %IntQueue*, %IntQueue** %r0
	%r11 = bitcast %IntQueue* %r10 to %IntQueue*
	call void @IntQueue__insert(%IntQueue* %r11,i32 7)
	%r12 = load %IntQueue*, %IntQueue** %r0
	%r13 = bitcast %IntQueue* %r12 to %IntQueue*
	%r14 = call i32 @IntQueue__first(%IntQueue* %r13)
	call void @printInt(i32 %r14)
	%r15 = load %IntQueue*, %IntQueue** %r0
	%r16 = bitcast %IntQueue* %r15 to %IntQueue*
	call void @IntQueue__rmFirst(%IntQueue* %r16)
	%r17 = load %IntQueue*, %IntQueue** %r0
	%r18 = bitcast %IntQueue* %r17 to %IntQueue*
	%r19 = call i32 @IntQueue__size(%IntQueue* %r18)
	call void @printInt(i32 %r19)
	ret i32 0
}

