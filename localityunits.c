#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdbool.h>

/* 1TB/8 = 128GB of direct space */
/* 2^37 direct size */
/* 2^37 / 2^12 = 2^25 = 32M */
#define TOTAL_SIZE    (1ULL << 34)
#define DIRECT_SIZE   (TOTAL_SIZE / 16)
#define BUCKET_COUNT  (DIRECT_SIZE / BUCKET_SIZE)
#define BUCKET_SIZE   (1ULL << 12)
#define COMPRESSION_RATIO 0.5

#define TOTAL_OBJECT_COUNT(locality_size)  ((TOTAL_SIZE - DIRECT_SIZE) / (locality_size))

#define REPEAT_COUNT  20

struct result 
{
    unsigned max;
    unsigned max_count;
    unsigned overflows;
    unsigned objects_in_overflowed_buckets;
};

static inline struct result simulate(unsigned compressed_locality_size, unsigned locality_ptrs_size)
{
    struct bucket 
    {
        unsigned obj_count;
        unsigned byte_count;
    };
    static struct bucket buckets[BUCKET_COUNT]; // Not thread safe!
    memset(buckets, 0, sizeof buckets);
    struct result res = { 0, 0, 0, 0 };
    unsigned iter = 0;
    for(iter = 0; iter < TOTAL_OBJECT_COUNT(compressed_locality_size); iter++) {
        unsigned index = random() % BUCKET_COUNT;
        bool old_overflow = buckets[index].byte_count > BUCKET_SIZE;
        buckets[index].byte_count += locality_ptrs_size;
        buckets[index].obj_count++;
        bool new_overflow = buckets[index].byte_count > BUCKET_SIZE;
        if(new_overflow) {
            if(old_overflow) {
                res.objects_in_overflowed_buckets++;
            } else {
                res.objects_in_overflowed_buckets += buckets[index].obj_count;
            }
            res.overflows++;
        }
        if(buckets[index].byte_count == res.max) {
            res.max_count++;
        } else if(buckets[index].byte_count > res.max) {
            res.max = buckets[index].byte_count;
            res.max_count = 1;
        }
    }
    return res;
}

static inline void simulate_repeatedly(void)
{
    printf("Bucket count = %llu, compression_ratio = %g\n",
           BUCKET_COUNT, COMPRESSION_RATIO);
    unsigned raw_locality_size;
    for(raw_locality_size = (1ULL<<16); raw_locality_size <= (1ULL<<20); raw_locality_size *= 2) {
        unsigned compressed_locality_size = raw_locality_size * COMPRESSION_RATIO;
        unsigned ptr_count = raw_locality_size >> 12;
        unsigned locality_ptrs_size = 15 + 8*ptr_count;

        double total_max = 0, total_overflows = 0, total_objects_in_overflowed_buckets = 0;
        unsigned i;
        for(i = 0; i < REPEAT_COUNT; i++) {
            struct result res = simulate(compressed_locality_size, locality_ptrs_size);
            total_max += res.max;
            total_overflows += res.overflows;
            total_objects_in_overflowed_buckets += res.objects_in_overflowed_buckets;
        }
        unsigned total_object_count = TOTAL_OBJECT_COUNT(compressed_locality_size);
        printf("Locality size: %7u, Object count: %6u, Avg bucket size: %7g => Max bucket size: %7g. Overflows = %8g Objects in overflowed = %g (%g%%)\n",
               raw_locality_size,
               total_object_count,
               1.0 * total_object_count * locality_ptrs_size / BUCKET_COUNT,
               total_max / REPEAT_COUNT,
               total_overflows / REPEAT_COUNT,
               total_objects_in_overflowed_buckets / REPEAT_COUNT,
               total_objects_in_overflowed_buckets / REPEAT_COUNT * 100.0 / total_object_count);
    }
}

int main()
{
    srandom(time(NULL));
    simulate_repeatedly();
    return 0;
}
