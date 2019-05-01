#ifndef INCLUDE_CILK_STATICS_H_
#define INCLUDE_CILK_STATICS_H_

// Extern declarations for non-static functions in Cilk not included in the
// headers
extern void Cilk_create_context(CilkContext**);
extern void Cilk_run_hooks(HookList* list);
extern void Cilk_free_global_state(CilkContext *const);
extern void Cilk_free_context(CilkContext *const);

static void init_variables(CilkContext *context)
{
     /* initialize all shared_ro variables declared above */
     INIT_PARAMETER1(active_size, USE_PARAMETER1(options->nproc));
     INIT_PARAMETER1(infofile, (FILE *)0); /*pointer to the stats. output file)*/
     INIT_PARAMETER1(pthread_stacksize, USE_PARAMETER1(options->pthread_stacksize));
     INIT_PARAMETER1(assertion_failed_msg,
		    "Assertion failed: %s line %d file %s\n"
		    "This is either a Cilk bug, or your program\n"
		    "has corrupted some Cilk internal data structure.\n");
     INIT_PARAMETER1(stack_overflow_msg,
		    "Cilk runtime system: Stack overflow.\n"
		    "Your program may have infinite recursion.\n");
}

static void Cilk_global_init(CilkContext *const context)
{
     init_variables(context);

     /* debug module */
     Cilk_debug_init(context);

     /* architecture-specific module */
     Cilk_arch_specific_init(); /* global init or init per cilk invokation ? */

     /* initialize barrier code */
     Cilk_barrier_init(context);

     /* run initialization hooks for library functions */
     Cilk_run_hooks(USE_PARAMETER1(Cilk_init_global_hooks)); /**???????? - where to put it ? What's that ? */

     /* Initialize stats code */
     Cilk_stats_init(context);

     /* Initialize timing */
     Cilk_timing_init(context);
}

static void init_sync_variables(CilkContext *const context)
{
    /* pthread_cond_t cond_init = PTHREAD_COND_INITIALIZER; */
     int res;

     USE_SHARED1(terminating) = 0;
     USE_SHARED1(nothing_to_do) = 1;
     USE_SHARED1(workers_done_counter) = 0;
     USE_SHARED1(workers_are_done) = 0;

     /* Create conditions and mutex*/
     res = pthread_cond_init(&USE_SHARED1(waiting_workers_cond), NULL);
     CILK_CHECK((res == 0), (context, NULL, "error in pthread_cond_init: %d returned \n", res));

      res = pthread_mutex_init(&USE_SHARED1(workers_mutex), NULL);
     CILK_CHECK((res == 0), (context, NULL, "error in pthread_mutex_init: %d returned \n", res));

      res = pthread_cond_init(&USE_SHARED1(workers_done_cond), NULL);
     CILK_CHECK((res == 0), (context, NULL, "error in pthread_cond_init: %d returned \n", res));

      res = pthread_cond_init(&USE_SHARED1(wakeup_first_worker_cond), NULL);
     CILK_CHECK((res == 0), (context, NULL, "error in pthread_cond_init: %d returned \n", res));

      res = pthread_cond_init(&USE_SHARED1(wakeup_other_workers_cond), NULL);
     CILK_CHECK((res == 0), (context, NULL, "error in pthread_cond_init: %d returned \n", res));
}

static void init_variables_2( CilkContext *const context )
{
     /* initialize all shared_ro variables declared above */
     INIT_PARAMETER1(start_time, Cilk_get_wall_time());
     INIT_PARAMETER1(num_threads, 0); /*for stats */
     INIT_PARAMETER1(num_steals, 0); /*for stats */
#if CILK_STATS
     INIT_PARAMETER1(max_stack_depth, 0);
#endif
}

static void Cilk_global_init_2(CilkContext *const context)
{
     init_variables_2(context);

     USE_SHARED1(critical_path) = 0;
     USE_SHARED1(total_work) = 0;

     /* initialize the statistics gatherer and timer  - ALSO ALLOCATING EACH TIME,
	  MAKE SURE THAT IT IS RELEASED EACH TIME> TODO - SEPARATE ALLOCATION AND INIT TO ZERO */
     Cilk_event_gathering_init(context);
     Cilk_time_gathering_init(context);

}

static CilkWorkerState *create_worker_state(CilkContext *const context, long id)
{
	CilkWorkerState *const ws = Cilk_malloc_fixed(sizeof(CilkWorkerState));
	ws->self = id;
	ws->context = context;

#ifdef CILK_USE_PERFCTR
	ws->perfctr_kstate = __cilk_vperfctr_init(0);
#endif

	Cilk_barrier_per_worker_init(ws);
	Cilk_arch_specific_per_worker_init();

	/* initialize scheduler per worker */
	Cilk_scheduler_per_worker_init(ws);

	/* run initialization hooks for library functions */
	Cilk_run_hooks(USE_PARAMETER(Cilk_init_per_worker_hooks));

	Cilk_internal_malloc_per_worker_init(ws);

	return ws;
}

static void Cilk_child_main(CilkChildParams *const childParams)
{
	int local_terminating = 0;

	int id = childParams->id;
	CilkContext *const context = childParams->context;

	/* Init worker state object */
	CilkWorkerState *const ws = create_worker_state(context, id);

	while( ! local_terminating ) {
		/* Now wait for invocation */
		Cilk_worker_wait_for_invocation(context, id, &local_terminating);

		if(! local_terminating) {
			if (id == 0)
        Cilk_scheduler(ws, USE_PARAMETER(invoke_main));
			else 
				Cilk_scheduler(ws, (Closure *) NULL);

			Cilk_worker_is_done(context, &local_terminating);
		}
	}

	/* Terminate */
	Cilk_internal_malloc_per_worker_cleanup(ws);

	Cilk_scheduler_per_worker_terminate(ws);

	Cilk_free(ws);
}

static char *smart_sprint_time(double x)
{
     static char buf[128];

     if (x < 1.0E-3)
	  sprintf(buf, "%f us", x * 1.0E6);
     else if (x < 1.0)
	  sprintf(buf, "%f ms", x * 1.0E3);
     else
	  sprintf(buf, "%f s", x);

     return buf;
}

static void print_all_statistics(CilkContext *const context)
{
	if (USE_PARAMETER1(options->statlevel) >= 1) {
		/* Print Header line for statistics */
		fprintf(USE_PARAMETER1(infofile),
			"\nRUNTIME SYSTEM STATISTICS:\n"
			"\n");
		/* level 1 and above: print wall clock, work and CP */
		fprintf(USE_PARAMETER1(infofile),
			"Wall-clock running time on %d processor%s: %s\n",
			USE_PARAMETER1(active_size),
			USE_PARAMETER1(active_size) > 1 ? "s" : "",
			smart_sprint_time(Cilk_wall_time_to_sec(
				Cilk_get_wall_time() - USE_PARAMETER1(start_time))));
		WHEN_CILK_TIMING({
			fprintf(USE_PARAMETER1(infofile), "Total work = %s\n",
			smart_sprint_time(Cilk_compute_work(context)));
		});

		/*
		 * hack: we don't consider the total work if it is too
		 * small.  The trick allows us to compile the rts with
		 * CILK_CRITICAL_PATH=something and the user program with
		 * CILK_CRITICAL_PATH=0
		 */
		WHEN_CILK_TIMING({
			if (Cilk_time_to_sec(USE_SHARED1(total_work)) > 0.001) {
				fprintf(USE_PARAMETER1(infofile),
					"Total work (accumulated) = %s\n",
          smart_sprint_time(
            Cilk_time_to_sec(USE_SHARED1(total_work))));
        fprintf(USE_PARAMETER1(infofile),
          "Span = %s\n",
          smart_sprint_time(
            Cilk_time_to_sec(USE_SHARED1(critical_path))));
        fprintf(USE_PARAMETER1(infofile),
          "Parallelism = %f\n",
          (double) USE_SHARED1(total_work) /
            (double) USE_SHARED1(critical_path));
      }
    });
  }

  if (USE_PARAMETER1(options->statlevel) >= 2) {
    /*
     *  level 2 and above: print statistics on work stealing etc
     */
    Cilk_print_rts_statistics(context);
    WHEN_CILK_TIMING({
      if (Cilk_time_to_sec(USE_SHARED1(total_work)) > 0.001
        && USE_PARAMETER1(num_threads) > 0) {
        fprintf(USE_PARAMETER1(infofile),
          "AVERAGE THREAD LENGTH = %s\n",
          smart_sprint_time(
            Cilk_time_to_sec(
              USE_SHARED1(total_work)) /
                (double) USE_PARAMETER1(num_threads)));
      }

      if (Cilk_time_to_sec(USE_SHARED1(total_work)) > 0.001
        && USE_PARAMETER1(num_steals) > 0) {
        fprintf(USE_PARAMETER1(infofile),
          "AVERAGE SUBCOMPUTATION LENGTH = %s\n",
          smart_sprint_time(
            Cilk_time_to_sec(
              USE_SHARED1(total_work)) /
                ((double) USE_PARAMETER1(num_steals) + 1.0)));
      }
      fprintf(USE_PARAMETER1(infofile),
        "MAX STACK DEPTH = %d\n",
        USE_PARAMETER1(max_stack_depth));

    });
  }
  if (USE_PARAMETER1(options->statlevel) >= 3) {
    /* level 3 and above: memory statistics */
    Cilk_internal_malloc_print_statistics(context);
  }
  if (USE_PARAMETER1(options->statlevel) >= 4) {
    /* level 4 and above: print work done by each processor */
    Cilk_print_time_statistics(context);
  }
  if (USE_PARAMETER1(options->statlevel) >= 5) {
    /* level 5 and above: print summary breakout of time */
    Cilk_summarize_time_statistics(context);
  }
  if (USE_PARAMETER1(options->statlevel) >= 6) {
    /* level 6 and above: print detailed breakout of time */
    Cilk_print_detailed_time_statistics(context);
  }
}

#endif
