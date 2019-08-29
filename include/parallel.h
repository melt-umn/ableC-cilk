#ifndef INCLUDE_PARALLEL_H_
#define INCLUDE_PARALLEL_H_

#include <stdio.h>
#include <errno.h>

#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/sysinfo.h>

typedef int thread_pool;

struct message_response {
  char sent;
  void* content;
};

typedef struct message_response* thread_message;

pthread_key_t thread_type;

struct thread_pool_info {
  int id, threads;
  void* hdlrArg;
  void (*msgHdlr)(void*, void*, thread_message);
  void (*cleanup)(void*);
};

struct thread_pool_list {
  struct thread_pool_info* list;
  int num, len; // num == number in list, len == length of list
  pthread_rwlock_t lck;
};

int insert_thread_pool(struct thread_pool_list* list, struct thread_pool_info item) {
  pthread_rwlock_rdlock(&(list->lck));
  while(list->num == list->len) {
    pthread_rwlock_unlock(&(list->lck));
    pthread_rwlock_wrlock(&(list->lck));
    if(list->num == list->len) {
      list->len *= 2;
      struct thread_pool_info* ptr = realloc(list->list, sizeof(struct thread_pool_info) * list->len);
      if(ptr == NULL) {
        list->len /= 2;
        pthread_rwlock_unlock(&(list->lck));
        return -1;
      }
      list->list = ptr;
    }
    pthread_rwlock_unlock(&(list->lck));
    pthread_rwlock_rdlock(&(list->lck));
  }
  list->list[list->num] = item;
  list->num++;
  pthread_rwlock_unlock(&(list->lck));
  return list->num - 1;
}

struct thread_pool_info* get_thread_pool_by_idx(struct thread_pool_list* list, int index) {
  if(index < 0 || index > list->num) {
    return NULL;
  }
  
  pthread_rwlock_rdlock(&(list->lck));
  struct thread_pool_info* res = &(list->list[index]);
  pthread_rwlock_unlock(&(list->lck));
  return res;
}

int get_thread_pool_by_id(struct thread_pool_list* list, int id) {
  pthread_rwlock_rdlock(&(list->lck));
  int cnt = list->num;
  for(int i = 0; i < cnt; i++) {
    if(id == list->list[i].id) {
      pthread_rwlock_unlock(&(list->lck));
      return i;
    }
  }
  pthread_rwlock_unlock(&(list->lck));
  return -1;
}

struct thread_list {
  pthread_t* list;
  int num, len; // num == number in list, len == length of list
  pthread_mutex_t lck;
};

int insert_thread(struct thread_list* list, pthread_t thd) {
  pthread_mutex_lock(&(list->lck));
  if(list->num == list->len) {
    list->len *= 2;
    pthread_t* ptr = realloc(list->list, sizeof(pthread_t) * list->len);
    if(ptr == NULL) {
      list->len /= 2;
      pthread_mutex_unlock(&(list->lck));
      return -1;
    }
    list->list = ptr;
  }
  list->list[list->num] = thd;
  list->num++;
  pthread_mutex_unlock(&(list->lck));
  return 0;
}

struct thread_pool_list thread_pools;
int nprocs, /*thdNum,*/ signo;
struct thread_list thread_list;

static void pthread_exit_wrapper(int signo) {
  pthread_exit(NULL);
}

int init_thread_system() {
  nprocs = get_nprocs_conf();

  int err = pthread_key_create(&thread_type, NULL);
  if(err != 0) {
    return err;
  }

  thread_pools.list = malloc(sizeof(struct thread_pool_info) * 8);
  if(thread_pools.list == NULL)
    return ENOMEM;
  
  thread_pools.len = 8;
  thread_pools.num = 0;

  pthread_rwlock_init(&(thread_pools.lck), NULL);

  thread_list.list = malloc(sizeof(pthread_t) * nprocs);
  if(thread_list.list == NULL) {
    free(thread_pools.list);
    return ENOMEM;
  }

  thread_list.len = nprocs;
  thread_list.num = 0;
  pthread_mutex_init(&(thread_list.lck), NULL);

  //thdNum = 0;

  signo = SIGRTMIN;

  if(signo > SIGRTMAX) {
    free(thread_pools.list);
    free(thread_list.list);
    return EINVAL;
  }

  struct sigaction handler;
  handler.sa_handler = pthread_exit_wrapper;
  sigfillset(&(handler.sa_mask));
  handler.sa_flags = 0;
  if(sigaction(signo, &handler, NULL) != 0) {
    free(thread_pools.list);
    free(thread_list.list);
    return EINVAL;
  }

  return 0;
}

void destroy_thread_system() {
  pthread_rwlock_wrlock(&(thread_pools.lck));
  
  // Cancel threads ?
  pthread_mutex_lock(&(thread_list.lck));
  for(int i = 0; i < thread_list.num; i++) {
    if(pthread_kill(thread_list.list[i], signo) != 0) {
      fprintf(stderr, "Error in destroy_thread_system()\n");
      exit(-1);
    }
    if(pthread_join(thread_list.list[i], NULL) != 0) {
      fprintf(stderr, "Error in destroy_thread_system()\n");
      exit(-1);
    }
  }
 
  for(int i = 0; i < thread_pools.num; i++) {
    thread_pools.list[i].cleanup(thread_pools.list[i].hdlrArg);
  }

  free(thread_pools.list);
  free(thread_list.list);
}

void setup_thread_system() {
  init_thread_system();

  int retVal = atexit(destroy_thread_system);
  if(retVal != 0) {
    fprintf(stderr, "Error in atexit setup from setup_thread_system\n");
    exit(-1);
  }
}

struct launcher_info {
  void (*func)(void*);
  void* arg;
  int poolNum;
};

void* thread_pool_launcher(void* ptr) {
  struct launcher_info* info = ptr;
  void (*func)(void*) = info->func;
  void* arg = info->arg;
  pthread_setspecific(thread_type, (void*)(intptr_t)info->poolNum);
  free(ptr);

  func(arg);

  return NULL;
}

int init_thread_pool(int n, int id, void (*func)(void*), void** args, 
  void (*msgHdlr)(void*, void*, thread_message), void (*cleanup)(void*), 
  void* hdlrArg, thread_pool* res) {
 
  if(n <= 0)
    return EINVAL;
  if(id == 0 || get_thread_pool_by_id(&thread_pools, id) != -1)
    return EINVAL;

  struct thread_pool_info info;
  info.id = id;
  info.threads = n;
  info.msgHdlr = msgHdlr;
  info.cleanup = cleanup;
  info.hdlrArg = hdlrArg;

  int result = insert_thread_pool(&thread_pools, info);
  if(result == -1) 
    return ENOMEM;

  pthread_attr_t attr;
  pthread_t thread;
  int errors;

  errors = pthread_attr_init(&attr);
  if(errors != 0) return errors;

  for(int i = 0; i < n; i++) {
    struct launcher_info* info = malloc(sizeof(struct launcher_info));
    if(info == NULL)
      exit(-1);
    info->poolNum = id;
    info->func = func;
    info->arg = args ? args[i] : NULL;

    errors = pthread_create(&thread, &attr, thread_pool_launcher, info);
    if(errors != 0)
      exit(-1);

    if(insert_thread(&thread_list, thread) != 0)
      exit(-1);
  }

  pthread_attr_destroy(&attr);

  if(res)
    *res = result;

  return 0;
}

int get_thread_pool(int id, thread_pool* res) {
  if(res == NULL)
    return EINVAL;
  int idx = get_thread_pool_by_id(&thread_pools, id);
  if(idx == -1)
    return EINVAL;
  *res = idx;
  return 0;
}

int get_thread_count(thread_pool pool) {
  struct thread_pool_info* pl = get_thread_pool_by_idx(&thread_pools, pool);
  if(pl == NULL)
    return -1;

  return pl->threads;
}

/*
 * Returns the number of the thread pool the current thread is in
 */
int get_current_threadpool() {
  void* res = pthread_getspecific(thread_type);
  return (int) (intptr_t)res;
}

int send_message(thread_pool dst, void* msg, thread_message* res) {
  if(res == NULL)
    return EINVAL;

  struct thread_pool_info* pool = get_thread_pool_by_idx(&thread_pools, dst);
  if(pool == NULL)
    return EINVAL;

  struct message_response* result = malloc(sizeof(struct message_response));
  if(result == NULL)
    return ENOMEM;
  result->sent = 0;
  result->content = NULL;

  pool->msgHdlr(pool->hdlrArg, msg, (thread_message) result);
  *res = (thread_message) result;
  return 0;
}

int send_response(thread_message reply, void* msg) {
  if(reply->sent == 0) {
    reply->content = msg;
    reply->sent = 1;
  } else {
    return EINVAL;
  }

  return 0;
}

void* receive_response(thread_message msg) {
  struct message_response* volatile ptr = msg;
  while(ptr->sent == 0) ;

  void* res = msg->content;
  free(msg);
  return res;
}

#endif // INCLUDE_PARALLEL_H_
