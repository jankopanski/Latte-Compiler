#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct string {
  size_t length;
  char text[];
};

static struct string _emptyString = { 0 };

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

struct string * readString() {
  struct string *str;
  char *tmp;
  ssize_t len = 0;
  len = getline(&tmp, (size_t *) &len, stdin);
  if (len <= 0) {
    str = &_emptyString;
  }
  else {
    if (tmp[len - 1] == '\n') --len;
    str = malloc(sizeof(size_t) + len);
    str->length = (size_t) len;
    memcpy(str->text, tmp, len);
  }
  free(tmp);
  return str;
}
