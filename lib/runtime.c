#include <stdio.h>

// struct string {
//   size_t length;
//   char text[];
// };
// struct string concat(struct string *str1, struct string *str2) {
//
// }

void printInt(int n) {
  printf("%d\n", n);
}

// void printString(struct string *str) {
//   printf("%s\n", str);
// }

void error() {
  printf("runtime error\n");
}

int readInt() {
  int n;
  scanf("%d\n", &n);
  return n;
}

// void readString(char *str) {
//   scanf("%s\n", str);
// }
