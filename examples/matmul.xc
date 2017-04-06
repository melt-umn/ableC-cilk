/* 
 * Rectangular matrix multiplication.
 *
 * See the paper ``Cache-Oblivious Algorithms'', by
 * Matteo Frigo, Charles E. Leiserson, Harald Prokop, and 
 * Sridhar Ramachandran, FOCS 1999, for an explanation of
 * why this algorithm is good for caches.
 *
 * Author: Matteo Frigo
 */
static const char *ident __attribute__((__unused__))
     = "$HeadURL: https://bradley.csail.mit.edu/svn/repos/cilk/5.4.3/examples/matmul.cilk $ $LastChangedBy: sukhaj $ $Rev: 517 $ $Date: 2003-10-27 10:05:37 -0500 (Mon, 27 Oct 2003) $";

/*
 * Copyright (c) 2003 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include <cilk-lib.cilkh>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define REAL float

extern int Cilk_rand(void);

void zero(REAL *A, int n)
{
     int i, j;
     
     for (i = 0; i < n; i++) {
	  for (j = 0; j < n; j++) {
	       A[i * n + j] = 0.0;
	  }
     }
}

void init(REAL *A, int n)
{
     int i, j;
     
     for (i = 0; i < n; i++) {
	  for (j = 0; j < n; j++) {
	       A[i * n + j] = (double)Cilk_rand();
	  }
     }
}

double maxerror(REAL *A, REAL *B, int n)
{
     int i, j;
     double error = 0.0;
     
     for (i = 0; i < n; i++) {
	  for (j = 0; j < n; j++) {
	       double diff = (A[i * n + j] - B[i * n + j]) / A[i * n + j];
	       if (diff < 0)
		    diff = -diff;
	       if (diff > error)
		    error = diff;
	  }
     }
     return error;
}

void iter_matmul(REAL *A, REAL *B, REAL *C, int n)
{
     int i, j, k;
     
     for (i = 0; i < n; i++)
	  for (k = 0; k < n; k++) {
	       REAL c = 0.0;
	       for (j = 0; j < n; j++)
		    c += A[i * n + j] * B[j * n + k];
	       C[i * n + k] = c;
	  }
}

/* FIXME: note: prototype was not required in original example */
cilk void rec_matmul(REAL *A, REAL *B, REAL *C, int m, int n, int p, int ld,
		     int add);
/*
 * A \in M(m, n)
 * B \in M(n, p)
 * C \in M(m, p)
 */
cilk void rec_matmul(REAL *A, REAL *B, REAL *C, int m, int n, int p, int ld,
		     int add)
{
     if ((m + n + p) <= 64) {
	  int i, j, k;
	  /* base case */
	  if (add) {
	       for (i = 0; i < m; i++)
		    for (k = 0; k < p; k++) {
			 REAL c = 0.0;
			 for (j = 0; j < n; j++)
			      c += A[i * ld + j] * B[j * ld + k];
			 C[i * ld + k] += c;
		    }
	  } else {
	       for (i = 0; i < m; i++)
		    for (k = 0; k < p; k++) {
			 REAL c = 0.0;
			 for (j = 0; j < n; j++)
			      c += A[i * ld + j] * B[j * ld + k];
			 C[i * ld + k] = c;
		    }
	  }
     } else if (m >= n && n >= p) {
	  int m1 = m >> 1;
	  spawn rec_matmul(A, B, C, m1, n, p, ld, add);
	  spawn rec_matmul(A + m1 * ld, B, C + m1 * ld, m - m1,
			   n, p, ld, add);
     } else if (n >= m && n >= p) {
	  int n1 = n >> 1;
	  spawn rec_matmul(A, B, C, m, n1, p, ld, add);
	  sync;
	  spawn rec_matmul(A + n1, B + n1 * ld, C, m, n - n1, p, ld, 1);
     } else {
	  int p1 = p >> 1;
	  spawn rec_matmul(A, B, C, m, n, p1, ld, add);
	  spawn rec_matmul(A, B + p1, C + p1, m, n, p - p1, ld, add);
     }
}

cilk int main(int argc, char **argv)
{
     int n;
     REAL *A, *B, *C1, *C2;
     double err;
     Cilk_time tm_begin, tm_elapsed;
     Cilk_time wk_begin, wk_elapsed;
     Cilk_time cp_begin, cp_elapsed;

     if (argc != 2) {
	  fprintf(stderr, "Usage: matmul [<cilk options>] <n>\n");
	  Cilk_exit(1);
     }
     n = atoi((const char *)argv[1]);

     A = malloc(n * n * sizeof(REAL));
     B = malloc(n * n * sizeof(REAL));
     C1 = malloc(n * n * sizeof(REAL));
     C2 = malloc(n * n * sizeof(REAL));
	  
     init(A, n);
     init(B, n);
     zero(C1, n);
     zero(C2, n);

     iter_matmul(A, B, C1, n);

     /* Timing. "Start" timers */
     sync;
     cp_begin = Cilk_user_critical_path;
     wk_begin = Cilk_user_work;
     tm_begin = Cilk_get_wall_time();

     spawn rec_matmul(A, B, C2, n, n, n, n, 0); 
     sync;

     /* Timing. "Stop" timers */
     tm_elapsed = Cilk_get_wall_time() - tm_begin;
     wk_elapsed = Cilk_user_work - wk_begin;
     cp_elapsed = Cilk_user_critical_path - cp_begin;

     err = maxerror(C1, C2, n);

     printf("\nCilk Example: matmul\n");
     printf("	      running on %d processor%s\n\n",
	    Cilk_active_size, Cilk_active_size > 1 ? "s" : "");
     printf("Max error     = %g\n", err);
     printf("Options: size = %d\n", n);
     printf("Running time  = %4f s\n", Cilk_wall_time_to_sec(tm_elapsed));
     printf("Work          = %4f s\n", Cilk_time_to_sec(wk_elapsed));
     printf("Critical path = %4f s\n", Cilk_time_to_sec(cp_elapsed));
     printf("``MFLOPS''    = %4f\n\n",
	    2.0 * n * n * n / (1.0e6 * Cilk_wall_time_to_sec(tm_elapsed)));

     free(C2);
     free(C1);
     free(B);
     free(A);
     cilk return 0;
}

#ifndef RAND_MAX
#define RAND_MAX 32767
#endif

static unsigned long rand_nxt = 0;
static Cilk_lockvar rand_lock;

int Cilk_rand(void)
{
     int result;
     Cilk_lock(rand_lock);
     rand_nxt = rand_nxt * 1103515245 + 12345;
     result = (rand_nxt >> 16) % ((unsigned int) RAND_MAX + 1);
     Cilk_unlock(rand_lock);
     return result;
}

void Cilk_srand(unsigned int seed)
{
     Cilk_lock(rand_lock);
     rand_nxt = seed;
     Cilk_unlock(rand_lock);
}

#if 0
static void rand_init_hook(void)
{
     Cilk_lock_init(rand_lock);
}

static void rand_init(void) __attribute__((constructor));
static void rand_init(void)
{
     Cilk_add_hook(&Cilk_init_global_hooks, rand_init_hook);
}
#endif
