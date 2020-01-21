#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

// clang -O0 -o runtime.ll -emit-llvm -S runtime.c

void printInt(int n) { printf("%d\n", n); }
void printString(char* s) { printf("%s\n", s); }
int readInt() { int n; scanf("%d\n", &n); return n;}
void error() { printf("runtimeError\n"); exit(1); }
char* readString() {
    char* line = NULL;
    size_t len = 0;
    getline(&line, &len,  stdin);
    line[strcspn(line, "\n")] = 0;
    return line;
}

char* __concatStrings(char* s1, char *s2) {
    char* result = malloc(strlen(s1) + strlen(s2) +1);
    strcpy(result, s1);
    strcat(result, s2);
    return result;
}

bool __compareStringsEQ(char* s1, char *s2) {
    return (bool) (strcmp(s1, s2) == 0);
}

bool __compareStringsNE(char* s1, char *s2) {
    return (bool) (strcmp(s1, s2) != 0);
}

int main() {
    int a = 0;
    int b = 1;
    int c = 2;
    int d = 3;
    int e = 4;

    while (a < 10) {
        a++;
        while (b < 10) {
            while (c < 10) {
                c++;
                while (d < 10) {
                    while (e < 10) {
                        e++;
                    }
                    d++;
                }
            }
            b++;
        }
    }

    printInt(a);
    printInt(b);
    printInt(c);
    printInt(d);
    printInt(e);

    printInt(a+b+c+d+e);
    return 0;
}