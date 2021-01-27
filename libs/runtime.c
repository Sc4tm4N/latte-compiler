#include <stdlib.h>
#include <string.h>
#include <stdio.h>

void printInt(int n) {
    printf("%d\n", n);
}

void printString(const char* str) {
    printf("%s\n", str);
}

int strcmp(const char *s1, const char *s2);

int readInt() {
    int n;
    scanf("%d\n", &n);
    return n;
}

char* readString() {
    char* result = (char*)malloc(1);
    size_t length;
    getline(&result, &length, stdin);
    length = strlen(result);
    result[length - 1] = '\0';
    return result;
}

void error() {
    printf("runtime error");
    exit(1);
}

char* concat(char* a, char *b) {
  size_t lena, lenb;
  lena = strlen(a);
  lenb = strlen(b);
  char* res = malloc(lena + lenb + 1);
  memcpy(res, a, lena);
  memcpy(res + lena, b, lenb);
  res[lena + lenb] = '\0';
  return res;
}