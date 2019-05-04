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
    jq->waiters = 0;
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
int job_queue_pop(job_queue* jq, job* j)
{
    mutex_lock(&jq->lock);
    if (jq->head == jq->tail) {
        jq->waiters++;
        do {
            if (jq->jobs == UREG_MAX) {
                mutex_unlock(&jq->lock);
                return JQ_DONE;
            }
            cond_var_wait(&jq->has_jobs, &jq->lock);
        } while (jq->head == jq->tail);
        jq->waiters--;
    }
    if (jq->jobs == UREG_MAX) {
        mutex_unlock(&jq->lock);
        return JQ_DONE;
    }
    *j = *jq->tail;
    job_queue_inc_ptr(jq, &jq->tail);
    jq->jobs--;
    mutex_unlock(&jq->lock);
    return OK;
}

int job_queue_push(job_queue* jq, const job* jb, ureg* waiters, ureg* jobs)
{
    mutex_lock(&jq->lock);
    if (jq->jobs == UREG_MAX) {
        mutex_unlock(&jq->lock);
        return JQ_DONE;
    }
    job* j = jq->head;
    job_queue_inc_ptr(jq, &jq->head);
    if (jq->head == jq->tail) {
        ureg size_old = ptrdiff(jq->buffer_end, jq->buffer);
        ureg size_new = size_old * 2;
        job* buffer_new = tmalloc(size_new);
        if (!buffer_new) {
            mutex_unlock(&jq->lock);
            return ERR;
        }
        ureg tail_size = ptrdiff(jq->buffer_end, jq->tail);
        memcpy(buffer_new, jq->tail, tail_size);
        memcpy(ptradd(buffer_new, tail_size), jq->buffer, size_old - tail_size);
        tfree(jq->buffer);
        jq->buffer = buffer_new;
        jq->buffer_end = ptradd(buffer_new, size_new);
        jq->tail = jq->buffer;
        jq->head = ptradd(jq->buffer, size_old);
        j = jq->head - 1;
    }
    *j = *jb;
    *waiters = jq->waiters;
    *jobs = jq->jobs++;
    mutex_unlock(&jq->lock);
    if (*waiters > 0) cond_var_notify_one(&jq->has_jobs);
    return OK;
}

void job_queue_stop(job_queue* jq)
{
    mutex_lock(&jq->lock);
    jq->jobs = UREG_MAX;
    mutex_unlock(&jq->lock);
    cond_var_notify_all(&jq->has_jobs);
}
