#include <stdlib.h>
#include <stdio.h>

#define BUFFER_SIZE 128

int main(int argc, char **argv) {
  fputs("Calculator by Jay\n", stdout);

  char buffer[BUFFER_SIZE];
  while (1) {
    fputs("$ ", stdout);
    fgets(buffer, BUFFER_SIZE, stdin);
  }

  return 0;
}
