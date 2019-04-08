#include "job_queue.h"
#include "tauc.h"

#define JOB_QUEUE_INITIAL_CAPACITY 1

int job_queue_init(job_queue* jq)
{
    if (mutex_init(&jq->lock)) return ERR;
    if (cond_var_init(&jq->has_jobs)) {
        mutex_fin(&jq->lock);
        return ERR;
    }
    ureg size = JOB_QUEUE_INITIAL_CAPACITY * sizeof(job);
    jq->buffer = tmalloc(size);
    if (!jq->buffer) {
        cond_var_fin(&jq->has_jobs);
        mutex_fin(&jq->lock);
        return ERR;
    }
    jq->buffer_end = ptradd(jq->buffer, size);
    jq->head = jq->buffer;
    jq->tail = jq->buffer;
    jq->idle_threads_count = 0;
    jq->threads_count = 1;
    return OK;
}
void job_queue_fin(job_queue* jq)
{
    tfree(jq->buffer);
    mutex_fin(&jq->lock);
}
static inline void job_queue_inc_ptr(job_queue* jq, job** ptr)
{
    (*ptr)++;
    if (*ptr == jq->buffer_end) *ptr = jq->buffer;
}
job_queue_result job_queue_pop(job_queue* jq, job* j)
{
    mutex_lock(&jq->lock);
    if (jq->head == jq->tail) {
        jq->idle_threads_count++;
        do {
            if (jq->idle_threads_count >= jq->threads_count) {
                mutex_unlock(&jq->lock);
                return JQR_DONE;
            }
            cond_var_wait(&jq->has_jobs, &jq->lock);
        } while (jq->head == jq->tail);
        jq->idle_threads_count--;
    }
    *j = *jq->tail;
    job_queue_inc_ptr(jq, &jq->tail);
    mutex_unlock(&jq->lock);
    return JQR_SUCCESS;
}

static inline job* job_queue_push_raw(job_queue* jq)
{
    job* res = jq->head;
    job_queue_inc_ptr(jq, &jq->head);
    if (jq->head == jq->tail) {
        ureg size_old = ptrdiff(jq->buffer_end, jq->buffer);
        ureg size_new = size_old * 2;
        job* buffer_new = tmalloc(size_new);
        if (!buffer_new) return NULL;
        ureg tail_size = ptrdiff(jq->buffer_end, jq->tail);
        memcpy(buffer_new, jq->tail, tail_size);
        memcpy(ptradd(buffer_new, tail_size), jq->buffer, size_old - tail_size);
        tfree(jq->buffer);
        jq->buffer = buffer_new;
        jq->buffer_end = ptradd(buffer_new, size_new);
        jq->tail = jq->buffer;
        jq->head = ptradd(jq->buffer, size_old);
        res = jq->head - 1;
    }
    return res;
}

static inline job_queue_result job_queue_push(job_queue* jq, job jb)
{
    bool reinforcements = false;
    mutex_lock(&jq->lock);
    job* j = job_queue_push_raw(jq);
    if (!j) {
        mutex_unlock(&jq->lock);
        return ERR;
    }
    *j = jb;
    if (jq->idle_threads_count == 0) {
        if (jq->threads_count < VIRT_CORE_COUNT) {
            reinforcements = true;
        }
        mutex_unlock(&jq->lock);
    }
    else {
        mutex_unlock(&jq->lock);
        cond_var_notify_one(&jq->has_jobs);
    }
    if (reinforcements) return JQR_SUCCESS_WITH_REINFORCEMENTS_REQUEST;
    return JQR_SUCCESS;
}

job_queue_result job_queue_request_parse(job_queue* jq, src_file* f)
{
    job j;
    j.type = JOB_PARSE;
    j.concrete.parse.file = f;
    return job_queue_push(jq, j);
}
job_queue_result job_queue_request_resolve(job_queue* jq, mdg_node* node)
{
    job j;
    j.type = JOB_RESOLVE;
    j.concrete.resolve.node = node;
    return job_queue_push(jq, j);
}
void job_queue_inform_thread_added(job_queue* jq)
{
    mutex_lock(&jq->lock);
    jq->threads_count++;
    mutex_unlock(&jq->lock);
}
void job_queue_force_done(job_queue* jq)
{
    mutex_lock(&jq->lock);
    jq->idle_threads_count = UREGH_MAX;
    mutex_unlock(&jq->lock);
    cond_var_notify_all(&jq->has_jobs);
}