/*
 * Cilk program to solve the 0-1 knapsack problem using a branch-and-bound
 * technique. 
 *
 * Author: Matteo Frigo
 */
static const char *ident __attribute__((__unused__))
     = "$HeadURL: https://bradley.csail.mit.edu/svn/repos/cilk/5.4.3/examples/knapsack.cilk $ $LastChangedBy: sukhaj $ $Rev: 517 $ $Date: 2003-10-27 10:05:37 -0500 (Mon, 27 Oct 2003) $";
/*
 * Copyright (c) 2000 Massachusetts Institute of Technology
 * Copyright (c) 2000 Matteo Frigo
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
#include <limits.h>
#include <getoptions.h>
#include <string.h>
#include <stdarg.h>

/* every item in the knapsack has a weight and a value */
#define MAX_ITEMS 256

struct item {
     int value;
     int weight;
};

int best_so_far = INT_MIN;

int compare(struct item *a, struct item *b)
{
     double c = ((double) a->value / a->weight) -
     ((double) b->value / b->weight);

     if (c > 0)
	  return -1;
     if (c < 0)
	  return 1;
     return 0;
}

int read_input(const char *filename, struct item *items, int *capacity, int *n)
{
     int i;
     FILE *f;

     if (filename == NULL)
	  filename = "\0";
     f = fopen(filename, "r");
     if (f == NULL) {
	  fprintf(stderr, "open_input(\"%s\") failed\n", filename);
	  return -1;
     }
     /* format of the input: #items capacity value1 weight1 ... */
     fscanf(f, "%d", n);
     fscanf(f, "%d", capacity);

     for (i = 0; i < *n; ++i)
	  fscanf(f, "%d %d", &items[i].value, &items[i].weight);

     fclose(f);

     /* sort the items on decreasing order of value/weight */
     /* cilk2c is fascist in dealing with pointers, whence the ugly cast */
     qsort(items, *n, sizeof(struct item),
	    (int (*)(const void *, const void *)) compare);

     return 0;
}

/* 
 * return the optimal solution for n items (first is e) and
 * capacity c. Value so far is v.
 */
cilk int knapsack(struct item *e, int c, int n, int v);
cilk int knapsack(struct item *e, int c, int n, int v)
{
     int with, without, best;
     double ub;

     /* base case: full knapsack or no items */
     if (c < 0)
	  cilk_return INT_MIN;

     if (n == 0 || c == 0)
	  cilk_return v;		/* feasible solution, with value v */

     ub = (double) v + c * e->value / e->weight;

     if (ub < best_so_far) {
	  /* prune ! */
	  cilk_return INT_MIN;
     }
     /* 
      * compute the best solution without the current item in the knapsack 
      */
     spawn without = knapsack(e + 1, c, n - 1, v);

     /* compute the best solution with the current item in the knapsack */
     spawn with = knapsack(e + 1, c - e->weight, n - 1, v + e->value);

     sync;

     best = with > without ? with : without;

     /* 
      * notice the race condition here. The program is still
      * correct, in the sense that the best solution so far
      * is at least best_so_far. Moreover best_so_far gets updated
      * when returning, so eventually it should get the right
      * value. The program is highly non-deterministic.
      */
     if (best > best_so_far)
	  best_so_far = best;

     cilk_return best;
}

int usage(void)
{
     fprintf(stderr, "\nUsage: knapsack [<cilk-options>] [-f filename] [-benchmark] [-h]\n\n");
     fprintf(stderr, "The 0-1-Knapsack is a standard combinatorial optimization problem: ``A\n");
     fprintf(stderr, "thief robbing a store finds n items; the ith item is worth v_i dollars\n");
     fprintf(stderr, "and weighs w_i pounds, where v_i and w_i are integers. He wants to take\n");
     fprintf(stderr, "as valuable a load as possible, but he can carry at most W pounds in\n");
     fprintf(stderr, "his knapsack for some integer W. What items should he take?''\n\n");
     return -1;
}

char *specifiers[] =
{"-f", "-benchmark", "-h", 0};
int opt_types[] =
{STRINGARG, BENCHMARK, BOOLARG, 0};

cilk int cilk_main(int argc, char **argv)
{
//     struct item items[MAX_ITEMS];	/* array of items */
     struct item *items = malloc(MAX_ITEMS * sizeof(*items));
     int n, capacity, sol, benchmark, help;
     Cilk_time tm_begin, tm_elapsed;
     Cilk_time wk_begin, wk_elapsed;
     Cilk_time cp_begin, cp_elapsed;
//     char filename[100];
     char *filename = malloc(100 * sizeof(*filename));

     /* standard benchmark options */
     strcpy(filename, "knapsack-example2.input");

     get_options(argc, argv, specifiers, opt_types, filename, &benchmark, &help);

     if (help)
	  cilk_return usage();

     if (benchmark) {
	  switch (benchmark) {
	      case 1:		/* short benchmark options -- a little work */
		   strcpy(filename, "knapsack-example1.input");
		   break;
	      case 2:		/* standard benchmark options */
		   strcpy(filename, "knapsack-example2.input");
		   break;
	      case 3:		/* long benchmark options -- a lot of work */
		   strcpy(filename, "knapsack-example3.input");
		   break;
	  }
     }
     if (read_input((const char *)filename, items, &capacity, &n))
	  cilk_return 1;

     /* Timing. "Start" timers */
     sync;
     cp_begin = Cilk_user_critical_path;
     wk_begin = Cilk_user_work;
     tm_begin = Cilk_get_wall_time();

     spawn sol = knapsack(items, capacity, n, 0);
     sync;

     /* Timing. "Stop" timers */
     tm_elapsed = Cilk_get_wall_time() - tm_begin;
     wk_elapsed = Cilk_user_work - wk_begin;
     cp_elapsed = Cilk_user_critical_path - cp_begin;

     printf("\nCilk Example: knapsack\n");
     printf("	      running on %d processor%s\n\n", Cilk_active_size, Cilk_active_size > 1 ? "s" : "");
     printf("options: problem-file = %s\n\n", filename);
     printf("Best value is %d\n\n", sol);
     printf("Running time  = %4f s\n", Cilk_wall_time_to_sec(tm_elapsed));
     printf("Work          = %4f s\n", Cilk_time_to_sec(wk_elapsed));
     printf("Critical path = %4f s\n\n", Cilk_time_to_sec(cp_elapsed));

     cilk_return 0;
}

void get_options(int argc, char *argv[], char *specs[], int *types,...)
{
     va_list ap;
     int type, i;
     int *intval;
     double *doubleval;
     long *longval;
     char *stringval;

     va_start(ap, types);

     while (((type = *types++) != 0) && (specs != 0)) {
	  switch (type) {
	      case INTARG:
		   intval = (int *) va_arg(ap, int *);
		   for (i = 1; i < (argc - 1); i++)
			if (!(strcmp(argv[i], specs[0]))) {
			     *intval = atoi((const char *)argv[i + 1]);
			     argv[i][0] = 0;
			     argv[i + 1][0] = 0;
			}
		   break;
	      case DOUBLEARG:
		   doubleval = (double *) va_arg(ap, double *);
		   for (i = 1; i < (argc - 1); i++)
			if (!(strcmp(argv[i], specs[0]))) {
			     *doubleval = atof((const char *)argv[i + 1]);
			     argv[i][0] = 0;
			     argv[i + 1][0] = 0;
			}
		   break;
	      case LONGARG:
		   longval = (long *) va_arg(ap, long *);
		   for (i = 1; i < (argc - 1); i++)
			if (!(strcmp(argv[i], specs[0]))) {
			     *longval = atol((const char *)argv[i + 1]);
			     argv[i][0] = 0;
			     argv[i + 1][0] = 0;
			}
		   break;
	      case BOOLARG:
		   intval = (int *) va_arg(ap, int *);
		   *intval = 0;
		   for (i = 1; i < argc; i++)
			if (!(strcmp(argv[i], specs[0]))) {
			     *intval = 1;
			     argv[i][0] = 0;
			}
		   break;
	      case STRINGARG:
		   stringval = (char *) va_arg(ap, char *);
		   for (i = 1; i < (argc - 1); i++)
			if (!(strcmp(argv[i], specs[0]))) {
			     strcpy(stringval, (const char *)argv[i + 1]);
			     argv[i][0] = 0;
			     argv[i + 1][0] = 0;
			}
		   break;
	      case BENCHMARK:
		   intval = (int *) va_arg(ap, int *);
		   *intval = 0;
		   for (i = 1; i < argc; i++) {
			if (!(strcmp(argv[i], specs[0]))) {
			     *intval = 2;
			     if ((i + 1) < argc) {
				  if (!(strcmp(argv[i + 1], "short")))
				       *intval = 1;
				  if (!(strcmp(argv[i + 1], "medium")))
				       *intval = 2;
				  if (!(strcmp(argv[i + 1], "long")))
				       *intval = 3;
				  argv[i + 1][0] = 0;
			     }
			     argv[i][0] = 0;
			}
		   }
		   break;
	  }
	  specs++;
     }
     va_end(ap);

     for (i = 1; i < argc; i++)
	  if (argv[i][0] != 0)
	       printf("\nInvalid option: %s\n", argv[i]);

}
