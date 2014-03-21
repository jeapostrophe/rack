#include <stdio.h>

int f () { return getc(0) + 1; }
int g () { return getc(0); }
int h () { return 6; }

int main () {
  int x = ( f() ? g() : h() );
  int y = ( f() ? 7 : 8 );
  return x + y;
}
