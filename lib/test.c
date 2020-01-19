#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// clang -O0 -o test.ll -emit-llvm -S test.c

struct a {
    int a;
};

int main() {
    char* s = "\\a\\n\n\tb\"";
    struct a *f = NULL;
    if (f == NULL) {
        printf("hello");
    }

    return 0;
}