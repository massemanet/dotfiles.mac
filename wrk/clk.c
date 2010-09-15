#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
int main() {
  long long start, stop;
  int i;
  struct timespec tp;

  if (clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &tp) < 0)
    exit(1);
  start = ((long long)tp.tv_sec * 1000000000LL) + (long long)tp.tv_nsec;
  for (i = 0; i < 100; i++)
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &tp);
  stop = ((long long)tp.tv_sec * 1000000000LL) + (long long)tp.tv_nsec;
    if (start == 0)
      exit(4);
  if (start == stop)
    exit(5);
  exit(0); return 0;
}

