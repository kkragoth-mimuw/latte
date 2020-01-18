#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// clang -O0 -o runtime.ll -emit-llvm -S runtime.c

struct a {
    int a;
};

int main() {
    struct a *f = NULL;
    return 0;
}