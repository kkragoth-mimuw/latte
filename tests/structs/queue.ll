declare i8* @malloc(i32) nounwind
declare void @printInt(i32)
declare void @printString(i8*)
declare void @error()
declare i32 @readInt()
declare i8* @readString()
declare i8* @__concatStrings(i8*, i8*)




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
	%r2 = call i8* @malloc(i32 12)
	%r3 = bitcast i8* %r2 to %Node*
	%r4 = alloca %Node*
	store %Node* %r3, %Node** %r4
	%r5 = load %Node*, %Node** %r4
	%r6 = load i32, i32* %r1
	call void @Node__setElem(%Node* %r5,i32 %r6)
	%r7 = load %IntQueue*, %IntQueue** %r0
	%r8 = call i1 @IntQueue__isEmpty(%IntQueue* %r7)
	br i1 %r8, label %L1, label %L2
L1:
	%r9 = load %IntQueue*, %IntQueue** %r0
	%r10 = getelementptr %IntQueue, %IntQueue* %r9, i32 0, i32 0
	%r11 = load %Node*, %Node** %r4
	store %Node* %r11, %Node** %r10
	br label %L3
L2:
	%r12 = load %IntQueue*, %IntQueue** %r0
	%r13 = getelementptr %IntQueue, %IntQueue* %r12, i32 0, i32 1
	%r14 = load %Node*, %Node** %r13
	%r15 = load %Node*, %Node** %r4
	call void @Node__setNext(%Node* %r14,%Node* %r15)
	br label %L3
L3:
	%r16 = load %IntQueue*, %IntQueue** %r0
	%r17 = getelementptr %IntQueue, %IntQueue* %r16, i32 0, i32 1
	%r18 = load %Node*, %Node** %r4
	store %Node* %r18, %Node** %r17
	ret void
}

define i32 @IntQueue__first(%IntQueue* %self) {
L0:
	%r0 = alloca %IntQueue*
	store %IntQueue* %self, %IntQueue** %r0
	%r1 = load %IntQueue*, %IntQueue** %r0
	%r2 = getelementptr %IntQueue, %IntQueue* %r1, i32 0, i32 0
	%r3 = load %Node*, %Node** %r2
	%r4 = call i32 @Node__getElem(%Node* %r3)
	ret i32 %r4
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
	%r6 = call %Node* @Node__getNext(%Node* %r5)
	store %Node* %r6, %Node** %r2
	ret void
}

define i32 @IntQueue__size(%IntQueue* %self) {
L0:
	%r0 = alloca %IntQueue*
	store %IntQueue* %self, %IntQueue** %r0
	%r1 = load %IntQueue*, %IntQueue** %r0
	%r2 = getelementptr %IntQueue, %IntQueue* %r1, i32 0, i32 0
	%r3 = load %Node*, %Node** %r2
	%r4 = alloca %Node*
	store %Node* %r3, %Node** %r4
	%r5 = alloca i32
	store i32 0, i32* %r5
	br label %L1
L1:
	%r6 = load %Node*, %Node** %r4
	%r7 = icmp ne %Node* %r6, null
	br i1 %r7, label %L2, label %L3
L2:
	%r8 = load %Node*, %Node** %r4
	%r9 = call %Node* @Node__getNext(%Node* %r8)
	store %Node* %r9, %Node** %r4
	%r10 = load i32, i32* %r5
	%r11 = add i32 %r10, 1
	store i32 %r11, i32* %r5
	br label %L1
L3:
	%r12 = load i32, i32* %r5
	ret i32 %r12
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
	%r0 = call i8* @malloc(i32 16)
	%r1 = bitcast i8* %r0 to %IntQueue*
	%r2 = alloca %IntQueue*
	store %IntQueue* %r1, %IntQueue** %r2
	%r3 = load %IntQueue*, %IntQueue** %r2
	%r4 = call i32 @f(i32 3)
	call void @IntQueue__insert(%IntQueue* %r3,i32 %r4)
	%r5 = load %IntQueue*, %IntQueue** %r2
	call void @IntQueue__insert(%IntQueue* %r5,i32 5)
	%r6 = load %IntQueue*, %IntQueue** %r2
	call void @IntQueue__insert(%IntQueue* %r6,i32 7)
	%r7 = load %IntQueue*, %IntQueue** %r2
	%r8 = call i32 @IntQueue__first(%IntQueue* %r7)
	call void @printInt(i32 %r8)
	%r9 = load %IntQueue*, %IntQueue** %r2
	call void @IntQueue__rmFirst(%IntQueue* %r9)
	%r10 = load %IntQueue*, %IntQueue** %r2
	%r11 = call i32 @IntQueue__size(%IntQueue* %r10)
	call void @printInt(i32 %r11)
	ret i32 0
}

