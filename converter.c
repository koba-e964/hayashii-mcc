#include <stdio.h>

int little_endian(char const *buf, int len) {
  int v = 0;
  int i;
  for (i = 0; i < len; ++i) {
    v += (buf[i] & 0xff) << (8 * i);
  }
  return v;
}

int main(void) {
  char buf[4];
  while (1) {
    int res = fread((void *) buf, 1, 4, stdin);
    if (res == 0) {
      break;
    }
    int v = little_endian(buf, res);
    printf("%d", v); // no newline, decimal
  }
  return 0;
}
