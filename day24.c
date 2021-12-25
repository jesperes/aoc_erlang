#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int i = 0;

uint64_t read_next(int *inputs) { return inputs[i++]; }

uint64_t alu(int *inputs) {
  i = 0;
  uint64_t x = 0;
  uint64_t y = 0;
  uint64_t w = 0;
  uint64_t z = 0;

  w = read_next(inputs);
  x *= 0;
  x += z;
  x %= 26;
  z /= 1;
  x += 11;
  x = (x == w) ? 1 : 0;
  x = (x == 0) ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 1;
  y *= x;
  z += y;
  w = read_next(inputs);
  x *= 0;
  x += z;
  x %= 26;
  z /= 1;
  x += 10;
  x = (x == w) ? 1 : 0;
  x = (x == 0) ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 10;
  y *= x;
  z += y;
  w = read_next(inputs);
  x *= 0;
  x += z;
  x %= 26;
  z /= 1;
  x += 13;
  x = (x == w) ? 1 : 0;
  x = (x == 0) ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 2;
  y *= x;
  z += y;
  w = read_next(inputs);
  x *= 0;
  x += z;
  x %= 26;
  z /= 26;
  x += -10;
  x = (x == w) ? 1 : 0;
  x = (x == 0) ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 5;
  y *= x;
  z += y;
  w = read_next(inputs);
  x *= 0;
  x += z;
  x %= 26;
  z /= 1;
  x += 11;
  x = (x == w) ? 1 : 0;
  x = (x == 0) ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 6;
  y *= x;
  z += y;
  w = read_next(inputs);
  x *= 0;
  x += z;
  x %= 26;
  z /= 1;
  x += 11;
  x = (x == w) ? 1 : 0;
  x = (x == 0) ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 0;
  y *= x;
  z += y;
  w = read_next(inputs);
  x *= 0;
  x += z;
  x %= 26;
  z /= 1;
  x += 12;
  x = (x == w) ? 1 : 0;
  x = (x == 0) ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 16;
  y *= x;
  z += y;
  w = read_next(inputs);
  x *= 0;
  x += z;
  x %= 26;
  z /= 26;
  x += -11;
  x = (x == w) ? 1 : 0;
  x = (x == 0) ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 12;
  y *= x;
  z += y;
  w = read_next(inputs);
  x *= 0;
  x += z;
  x %= 26;
  z /= 26;
  x += -7;
  x = (x == w) ? 1 : 0;
  x = (x == 0) ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 15;
  y *= x;
  z += y;
  w = read_next(inputs);
  x *= 0;
  x += z;
  x %= 26;
  z /= 1;
  x += 13;
  x = (x == w) ? 1 : 0;
  x = (x == 0) ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 7;
  y *= x;
  z += y;
  w = read_next(inputs);
  x *= 0;
  x += z;
  x %= 26;
  z /= 26;
  x += -13;
  x = (x == w) ? 1 : 0;
  x = (x == 0) ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 6;
  y *= x;
  z += y;
  w = read_next(inputs);
  x *= 0;
  x += z;
  x %= 26;
  z /= 26;
  x += 0;
  x = (x == w) ? 1 : 0;
  x = (x == 0) ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 5;
  y *= x;
  z += y;
  w = read_next(inputs);
  x *= 0;
  x += z;
  x %= 26;
  z /= 26;
  x += -11;
  x = (x == w) ? 1 : 0;
  x = (x == 0) ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 6;
  y *= x;
  z += y;
  w = read_next(inputs);
  x *= 0;
  x += z;
  x %= 26;
  z /= 26;
  x += 0;
  x = (x == w) ? 1 : 0;
  x = (x == 0) ? 1 : 0;
  y *= 0;
  y += 25;
  y *= x;
  y += 1;
  z *= y;
  y *= 0;
  y += w;
  y += 15;
  y *= x;
  z += y;

  // printf("z = %ld\n", z);

  return z;
}

int selftest() {
  {
    int inputs[] = {2, 4, 7, 1, 2, 2, 9, 8, 7, 5, 6, 8, 8, 8};
    assert(1095971003LL == alu(inputs));
  }
  {
    int inputs[] = {5, 8, 2, 7, 7, 8, 7, 8, 6, 4, 1, 6, 1, 5};
    assert(2073077598LL == alu(inputs));
  }
  {
    int inputs[] = {4, 1, 7, 1, 7, 2, 2, 8, 9, 5, 3, 3, 9, 9};
    assert(1678246360LL == alu(inputs));
  }
  {
    int inputs[] = {8, 9, 6, 9, 5, 4, 3, 2, 3, 9, 9, 3, 6, 2};
    assert(3012582317LL == alu(inputs));
  }
  {
    int inputs[] = {9, 6, 7, 1, 8, 4, 8, 9, 4, 4, 2, 8, 9, 2};
    assert(126240417LL == alu(inputs));
  }
  {
    int inputs[] = {9, 9, 3, 5, 5, 1, 5, 7, 4, 4, 7, 1, 3, 8};
    assert(3319668193LL == alu(inputs));
  }
  {
    int inputs[] = {7, 2, 3, 2, 9, 1, 8, 4, 8, 1, 1, 7, 9, 1};
    assert(100667934LL == alu(inputs));
  }
  {
    int inputs[] = {9, 7, 9, 4, 7, 2, 6, 6, 8, 3, 2, 7, 6, 1};
    assert(3295484390LL == alu(inputs));
  }
  {
    int inputs[] = {9, 4, 7, 7, 6, 5, 3, 7, 4, 7, 7, 9, 9, 2};
    assert(3261195539LL == alu(inputs));
  }
  {
    int inputs[] = {5, 7, 1, 7, 3, 3, 8, 4, 9, 6, 8, 3, 6, 5};
    assert(79273942LL == alu(inputs));
  }
  {
    int inputs[] = {2, 4, 2, 2, 9, 7, 3, 4, 4, 8, 9, 9, 8, 1};
    assert(1096554306LL == alu(inputs));
  }
  {
    int inputs[] = {2, 7, 1, 3, 8, 4, 7, 5, 3, 2, 4, 5, 6, 9};
    assert(1132635788LL == alu(inputs));
  }
  {
    int inputs[] = {6, 9, 3, 7, 5, 3, 6, 9, 5, 2, 9, 6, 2, 3};
    assert(2393836190LL == alu(inputs));
  }
  {
    int inputs[] = {8, 4, 4, 5, 1, 3, 3, 5, 8, 7, 6, 5, 3, 5};
    assert(2951276686LL == alu(inputs));
  }
  {
    int inputs[] = {3, 4, 8, 2, 9, 8, 5, 6, 9, 4, 4, 6, 1, 2};
    assert(1405470889LL == alu(inputs));
  }
  {
    int inputs[] = {3, 2, 4, 9, 6, 8, 9, 7, 6, 9, 1, 5, 7, 7};
    assert(1384854168LL == alu(inputs));
  }
  {
    int inputs[] = {9, 9, 9, 9, 2, 5, 8, 2, 2, 8, 1, 4, 4, 5};
    assert(3321446018LL == alu(inputs));
  }
  {
    int inputs[] = {3, 5, 7, 1, 6, 4, 2, 9, 1, 9, 6, 8, 9, 6};
    assert(1416839653LL == alu(inputs));
  }
  {
    int inputs[] = {1, 1, 4, 8, 1, 3, 5, 7, 5, 2, 9, 7, 7, 5};
    assert(754592976LL == alu(inputs));
  }
}