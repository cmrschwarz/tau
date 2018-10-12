void panic(){
    //raise a segfault to indicate a critical error
    *(int*)((void*)(0)) = 42;
}