#include <stddef.h>
// Runtime library for yabo, right now only needed for the wasm32-unknown-unknown target.
// These are required as llvm may syntheize calls to these functions.
void *memcpy(void *restrict dest, const void *restrict src, size_t n) {
  char *d = dest;
  const char *s = src;
  while (n--)
    *d++ = *s++;
  return dest;
}

void *memset(void *s, int c, size_t n) {
  char *p = s;
  while (n--)
    *p++ = c;
  return s;
}

void *memmove(void *dest, const void *src, size_t n) {
  if (dest < src) {
    char *d = dest;
    const char *s = src;
    while (n--)
      *d++ = *s++;
  } else {
    char *d = (char*)dest + n;
    const char *s = (const char*)src + n;
    while (n--)
      *--d = *--s;
  }
  return dest;
}

int memcmp(const void *s1, const void *s2, size_t n) {
  const unsigned char *p1 = s1, *p2 = s2;
  while (n--) {
    if (*p1 != *p2)
      return *p1 - *p2;
    p1++;
    p2++;
  }
  return 0;
}
