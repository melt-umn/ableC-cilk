#ifndef INCLUDE_CILK_ABLEC_H_
#define INCLUDE_CILK_ABLEC_H_

#include <stdlib.h>
#include <pthread.h>
#include "parallel.h"
#include <cilk.h>
#include <cilk-internal.h>

#include "cilk-statics.h"

struct cilk_message {
  void* args;
  CilkProcInfo* sig;
};

void cilk_message_handler(void* cntxt, void* in, thread_message rsp) {
  struct cilk_message* msg = in;
  CilkContext* context = cntxt;

  Closure* t;
  CilkStackFrame* f = msg->args;

  t = Cilk_Closure_create_malloc(context, NULL);
  t->parent = NULL;
  t->join_counter = 0;
  t->status = CLOSURE_READY;

  f->entry = 0;
  WHEN_CILK_DEBUG(f->magic = CILK_STACKFRAME_MAGIC);
  f->sig = msg->sig;

  t->frame = f;

  int target = rand() % USE_PARAMETER1(active_size);
  Cilk_mutex_wait(context, NULL, &USE_PARAMETER1(deques)[target].mutex);
  t->next_ready = USE_PARAMETER1(deques)[target].top;
  t->prev_ready = NULL;
  USE_PARAMETER1(deques)[target].top = t;

  if(USE_PARAMETER1(deques)[target].bottom) {
    (t->next_ready)->prev_ready = t;
  } else {
    USE_PARAMETER1(deques)[target].bottom = t;
  }
  Cilk_mutex_signal(context, &USE_PARAMETER1(deques)[target].mutex);

  context->Cilk_global_state->nothing_to_do = 0;
  pthread_cond_broadcast(&(context->Cilk_global_state->wakeup_first_worker_cond));

  send_response(rsp, NULL);
}

void Cilk_ableC_terminate(CilkContext* const context) {
  Cilk_free(USE_SHARED1(thrd_params_array));
  
  print_all_statistics(context);
  Cilk_scheduler_terminate(context);
  Cilk_timing_terminate(context);
  Cilk_stats_terminate(context);
  Cilk_barrier_terminate(context);
  Cilk_debug_terminate(context);
  Cilk_free_global_state(context);
  Cilk_free_context(context);
}

void cilk_cleanup(void* arg) {
  CilkContext* context = arg;
  context->Cilk_global_state->done = 1;
  Cilk_ableC_terminate(context);
}

void init_cilk_ableC(int threads) {
  CilkContext* context;

  Cilk_create_context(&context);

  Cilk_options cilk_default_options = CILK_DEFAULT_OPTIONS;
  *USE_PARAMETER1(options) = cilk_default_options;
  USE_PARAMETER1(options)->nproc = threads;

#ifdef HAVE_SCHED_SETAFFINITY
  if(USE_PARAMETER1(options)->pinned_mask > 1023) {
    /* do nothing (use the mask that was inherited) */
  } else {
    if(sched_setaffinity(0, sizeof(USE_PARAMETER1(options)->pinned_mask), &(USE_PARAMETER1(options)->pinned_mask))) {
      fprintf(stderr, "Failed pinning process, continuing on default mask...\n");
    }
  }
#endif

  Cilk_global_init(context);

  Cilk_scheduler_init(context);

  long i;
  int res;
  pthread_attr_t attr;

#ifdef CILK_USE_PERFCT
  __cilk_vperfct_init(1);
#endif

  init_sync_variables(context);

  USE_SHARED1(tid) = NULL;

  USE_SHARED1(thrd_params_array) =
    Cilk_malloc_fixed(USE_PARAMETER1(active_size) * sizeof(CilkChildParams));
  CILK_CHECK(USE_SHARED1(thrd_params_array), (context, NULL, "could not malloc params_array\n"));

  for(int i = 0; i < USE_PARAMETER1(active_size); i++) {
    USE_SHARED1(thrd_params_array)[i].context = context;
    USE_SHARED1(thrd_params_array)[i].id = i;
  }

  CilkChildParams** args = Cilk_malloc(USE_PARAMETER1(active_size) * sizeof(CilkChildParams*));
  for(int i = 0; i < USE_PARAMETER1(active_size); i++) {
    args[i] = &(USE_SHARED1(thrd_params_array)[i]);
  }

  init_thread_pool(threads, 2/*TODO: USE CILK ID*/, (void (*)(void*))Cilk_child_main,
      (void**) args, cilk_message_handler, cilk_cleanup, context, NULL);

  Cilk_free(args);

  Cilk_global_init_2(context);
  Cilk_scheduler_init_2(context);
  context->Cilk_RO_params->invoke_main = NULL;
}

#endif
