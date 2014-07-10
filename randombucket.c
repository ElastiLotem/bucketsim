#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUCKET_COUNT 1024
#define REPEAT_COUNT 100

static inline unsigned simulate(unsigned bucket_count) 
{
    int buckets[bucket_count];
    memset(buckets, 0, sizeof buckets);
    int empty_buckets = bucket_count;
    unsigned iter = 0;
    for(iter = 0; empty_buckets > 0; iter++) {
        int *bucket = &buckets[random() % bucket_count];
        if(0 == *bucket) empty_buckets--;
        (*bucket)++;
    }
    return iter;
}

static inline void simulate_repeatedly(unsigned bucket_count)
{
    unsigned total_iter = 0;
    unsigned i;
    for(i = 0; i < REPEAT_COUNT; i++) {
        total_iter += simulate(bucket_count);
    }
    double average_iter = total_iter * 1.0d / REPEAT_COUNT;

    printf("====== buckets = %u\n", bucket_count);
    printf("        %g iterations\n", average_iter);
    printf("    N * %g\n", average_iter / bucket_count);
    printf("logN  = %g\n", log(bucket_count));
    printf("ratio = %g\n", average_iter / bucket_count / log(bucket_count));

}

int main()
{
    unsigned i;
    unsigned bucket_count = 1024;
    for(i = 0; i < 10; i++) {
        simulate_repeatedly(bucket_count);
        bucket_count *= 2;
    }
    return 0;
}
