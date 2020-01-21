# PATH=$PATH:/usr/local/opt/llvm/bin/

import os
import subprocess
import sys

def generate_test_from_c_file(filePath):
    filePathBasename = filePath.partition(".")[0]
    subprocess.run(["gcc", filePath])
    with open(filePath, 'r') as cFile: 
        code = cFile.read()
        with open(filePathBasename + ".lat", 'w') as lat_file:
            lat_file.write(INTRO + code)
    with open(filePathBasename + ".output", 'w') as output_file:
        subprocess.run("./a.out", stdout=output_file)

if __name__ == "__main__":
    if len(sys.argv) > 0:
        generate_test_from_c_file(sys.argv[0])

INTRO = '''
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

'''