%myStruct = type { i32, %myStruct* }

define %myStruct @main() {
  ret %myStruct { i32 1, %myStruct* null }
}

; create a 2-node linked list of the %myStruct type
define %myStruct @linked_list() {
    %node1 = alloca %myStruct
    %node2 = alloca %myStruct
    
    %node1.i32 = getelementptr %myStruct, %myStruct* %node1, i32 0, i32 0
    store i32 1, i32* %node1.i32
    
    %node1.next = getelementptr %myStruct, %myStruct* %node1, i32 0, i32 1
    store %myStruct* %node2, %myStruct** %node1.next
    
    %node2.i32 = getelementptr %myStruct, %myStruct* %node2, i32 0, i32 0
    store i32 2, i32* %node2.i32
    
    %node2.next = getelementptr %myStruct, %myStruct* %node2, i32 0, i32 1
    store %myStruct* null, %myStruct** %node2.next
    
    ret %myStruct* %node1
}