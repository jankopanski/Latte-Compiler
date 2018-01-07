#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct string {
  size_t length;
  char text[];
};

struct string * _allocString(char *str, size_t len) {
  struct string *res = malloc(sizeof(size_t) + len);
  res->length = len;
  memcpy(res->text, str, len);
  return res;
}

void printInt(int n) {
  printf("%d\n", n);
}

void printString(struct string *str) {
  printf("%s\n", str->text);
}

void error() {
  printf("runtime error\n");
}

int readInt() {
  int n;
  scanf("%d\n", &n);
  return n;
}

// void readString(struct string *str) {
//   scanf("%s\n", str);
// }
