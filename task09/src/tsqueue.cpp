#include "tsqueue.h"

void threadsafe_queue_init(ThreadsafeQueue *q) {
    pthread_mutex_init(&q->mutex, NULL);
    pthread_cond_init(&q->non_empty, NULL);
    queue_init(&q->q);
}

void threadsafe_queue_destroy(ThreadsafeQueue *q) {
    queue_destroy(&q->q);
    pthread_cond_destroy(&q->non_empty);
    pthread_mutex_destroy(&q->mutex);
}

void threadsafe_queue_push(ThreadsafeQueue *q, void *data) {
    pthread_mutex_lock(&q->mutex);
    queue_push(&q->q, data);
    pthread_cond_signal(&q->non_empty);
    pthread_mutex_unlock(&q->mutex);
}

void *threadsafe_queue_wait_and_pop(ThreadsafeQueue *q) {
    pthread_mutex_lock(&q->mutex);
    while (queue_empty(&q->q))
        pthread_cond_wait(&q->non_empty, &q->mutex);

    void *return_value = queue_pop(&q->q);
    pthread_mutex_unlock(&q->mutex);
    return return_value;
}
