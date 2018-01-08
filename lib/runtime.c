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

struct string * _concatString(struct string * str1, struct string * str2) {
  struct string *res = malloc(sizeof(size_t) + str1->length + str2->length);
  res->length = str1->length + str2->length;
  memcpy(res->text, str1->text, str1->length);
  memcpy(res->text + str1->length, str2->text, str2->length);
  return res;
}

int _cmpString(struct string * str1, struct string * str2) {
  return !strcmp(str1->text, str2->text);
}

void printInt(int n) {
  printf("%d\n", n);
}

void printString(struct string *str) {
  fwrite(str->text, sizeof(char), str->length, stdout);
  putchar('\n');
}

void error() {
  printf("runtime error\n");
  exit(EXIT_FAILURE);
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
  if (len < 0) {
    error();
  }
  if (len == 0) {
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
