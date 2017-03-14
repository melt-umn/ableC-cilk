#include <stdio.h>

//int actives[2];
int a;

cilk int refine(void) {
	cilk_return 999;
}

cilk int main(void) {
    /* FIXME: allow lhs to be something other than id */
//	spawn actives[2] = refine();
	spawn a = refine();
//	if( actives[2] == 999 ) {
	if( a == 999 ) {
		printf("velin OK\n");
        }
	else {
		printf("velin BAD\n");
	}

	cilk_return 0;
}
