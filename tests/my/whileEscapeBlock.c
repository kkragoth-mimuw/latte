#include <stdlib.h>
#include <stdio.h>
void printInt(int n) { printf("%d\n", n); }

int main() {
    int a = 0;
    int b = 0;
    int c = 0;
    int d = 1;
    if (a < d) {
        while (b < 5) {
            b++;
            while (c < 5) {
                c++;
                a = a + 2;
            }
        }
    }
    printInt(a);
    return 0;
} 