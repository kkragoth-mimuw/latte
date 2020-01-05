#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// clang -O0 -o runtime.ll -emit-llvm -S runtime.c

void printInt(int n) { printf("%d\n", n); }
void printString(char* s) { printf("%s\n", s); }
int readInt() { int n; scanf("%d\n", &n); return n;}
void error() { printf("runtimeError\n"); exit(1); }
// char* readString() {
//     memset(__readBuffer, '\0', 200001);
//     fgets(__readBuffer, 200000, stdin);

//     char *result = malloc(strlen(__readBuffer));
//     strcpy(result, __readBuffer);
//     return result;
// }

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

int main() {
    char* s1 = readString();
    char* s2 = readString();
    printString(__concatStrings(s1, s2));
}