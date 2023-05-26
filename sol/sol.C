
#include "stdio.h"
#include "stdlib.h"
#include <set>

typedef unsigned char pos; // 0..32
typedef unsigned long state; // using just least significant 33 bits

// Construct a state for a single position x.
#define p(x) (1L<<x)

// Move structure: x,y,z are the position indexes (0..32); used for printing
// The move can be played either x/y-->z OR z/y-->x.
// a,b,c are produced from x,y,z using the p macro. (we dont need b)
// ab,bc,abc are the required bitwise-or combinations of a,b,c.
// a,c,ab,bc are used as bit masks; passed to the filled/available macros
// abc is used to apply the move to the state, using xor(^)
typedef struct { pos x,y,z; state a,c, ab,bc, abc; } move;

// The m macro is the constructor for move
#define m(x,y,z) { x,y,z, p(x), p(z), p(x)|p(y), p(y)|p(z), p(x)|p(y)|p(z) }

#define filled(s,mask) ((s|mask)==s)
#define available(s,mask) ((s&mask)==0)
#define apply_move(s,mask) (s^m.abc)

#define NUM_MOVES 38

const move moves[NUM_MOVES] =
  {                           m( 0, 1, 2)
  ,                           m( 3, 4, 5)
  , m( 6, 7, 8), m( 7, 8, 9), m( 8, 9,10), m( 9,10,11), m(10,11,12)
  , m(13,14,15), m(14,15,16), m(15,16,17), m(16,17,18), m(17,18,19)
  , m(20,21,22), m(21,22,23), m(22,23,24), m(23,24,25), m(24,25,26)
  ,                           m(27,28,29)
  ,                           m(30,31,32)

  ,                           m( 6,13,20)
  ,                           m( 7,14,21)
  , m( 0, 3, 8), m( 3, 8,15), m( 8,15,22), m(15,22,27), m(22,27,30)
  , m( 1, 4, 9), m( 4, 9,16), m( 9,16,23), m(16,23,28), m(23,28,31)
  , m( 2, 5,10), m( 5,10,17), m(10,17,24), m(17,24,29), m(24,29,32)
  ,                           m(11,18,25)
  ,                           m(12,19,26)
  };

//const state init = p(0)|p(3)|p(4)|p(7)|p(8)|p(9)|p(10); // just 7 marbles
const state init = 0x1fffeffff; // all 32 marbles with center hole
const state goal = p(16); // just center hole filled

static int steps = 0;
static int played_index[31];
static bool played_foreward[31];

static std::set<state> visited;

static void search(int depth, state s) {
  steps++;
#if 0
  if (steps % 100000 == 0) {
    printf("search: (%d) d=%i, %09lx, #v=%ld\n",steps,depth,s,visited.size());
  }
#endif
  if (1 == visited.count(s)) return;
  if (s == goal) {
    printf("Solution found in %d steps\n",steps);
    for (int i = 0; i < depth; i++) {
      move m = moves[played_index[i]];
      if (played_foreward[i]) {
        printf("(%d) %d/%d-->%d\n",i+1,m.x,m.y,m.z);
      } else {
        printf("(%d) %d/%d-->%d\n",i+1,m.z,m.y,m.x);
      }
    }
    exit(0);
  }
  for (int i = 0; i < NUM_MOVES; i++) {
    move m = moves[i];
    bool legal = filled(s,m.ab) && available(s,m.c);
    if (legal) {
      played_index[depth] = i;
      played_foreward[depth] = true;
      search(depth+1, apply_move(s,m));
    }
  }
  for (int i = 0; i < NUM_MOVES; i++) {
    move m = moves[i];
    bool legal2 = filled(s,m.bc) && available(s,m.a);
    if (legal2) {
      played_index[depth] = i;
      played_foreward[depth] = false;
      search(depth+1, apply_move(s,m));
    }
  }
  visited.insert(s);
}

int main() {
  search(0,init);
  printf("no solutions found\n");
  exit(0);
}
