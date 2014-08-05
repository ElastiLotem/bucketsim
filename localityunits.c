#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdbool.h>

#define TOTAL_SIZE    (1ULL << 40)
#define DIRECT_SIZE   (TOTAL_SIZE / 16)
#define BUCKET_COUNT  (DIRECT_SIZE / BUCKET_SIZE)
#define BUCKET_SIZE   (4ULL << 10)
#define OVERFLOW_THRESHOLD (4ULL << 10)
#define COMPRESSION_RATIO 0.5

#define TOTAL_OBJECT_COUNT(locality_size)  ((TOTAL_SIZE - DIRECT_SIZE) / (locality_size))

#define REPEAT_COUNT  (128)

#define MAX(x, y)   ((x) > (y) ? (x) : (y))

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
        bool old_overflow = buckets[index].byte_count > OVERFLOW_THRESHOLD;
        buckets[index].byte_count += locality_ptrs_size;
        buckets[index].obj_count++;
        bool new_overflow = buckets[index].byte_count > OVERFLOW_THRESHOLD;
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
    printf("Total size = %llu MB, Bucket count = %llu, bucket size = %lluKB, compression_ratio = %g\n",
           TOTAL_SIZE >> 20, BUCKET_COUNT, BUCKET_SIZE >> 10, COMPRESSION_RATIO);
    unsigned raw_locality_size;
    for(raw_locality_size = (1ULL<<19); raw_locality_size <= (1ULL<<19); raw_locality_size *= 2) {
        unsigned compressed_locality_size = raw_locality_size * COMPRESSION_RATIO;
        unsigned ptr_count = raw_locality_size >> 12;
        unsigned locality_ptrs_size = 15 + 12*ptr_count;

        double total_max = 0, total_overflows = 0, total_objects_in_overflowed_buckets = 0;
        unsigned worst_max = 0, worst_overflows = 0, worst_objects_in_overflowed_buckets = 0;
        unsigned i;
        for(i = 0; i < REPEAT_COUNT; i++) {
            struct result res = simulate(compressed_locality_size, locality_ptrs_size);
            total_max += res.max;
            worst_max = MAX(worst_max, res.max);
            total_overflows += res.overflows;
            worst_overflows = MAX(worst_overflows, res.overflows);
            total_objects_in_overflowed_buckets += res.objects_in_overflowed_buckets;
            worst_objects_in_overflowed_buckets =
                MAX(worst_objects_in_overflowed_buckets, res.objects_in_overflowed_buckets);
        }
        unsigned total_object_count = TOTAL_OBJECT_COUNT(compressed_locality_size);
        printf("Locality size: %7u, Object count: %6u, Avg bucket util.: %7g, %u runs:\n"
               "    Avg Max bucket util.: %7g.   Avg Overflows = %8g Objects in overflowed = %g (%g%%)\n"
               "  Worst Max bucket util.: %7u. Worst Overflows = %8u Worst Objects in overflowed = %u (%g%%)\n",
               raw_locality_size, total_object_count,
               1.0 * total_object_count * locality_ptrs_size / BUCKET_COUNT,
               REPEAT_COUNT,
               /* Avg's */
               total_max / REPEAT_COUNT,
               total_overflows / REPEAT_COUNT,
               total_objects_in_overflowed_buckets / REPEAT_COUNT,
               total_objects_in_overflowed_buckets / REPEAT_COUNT * 100.0 / total_object_count,
               /* Worst's */
               worst_max,
               worst_overflows,
               worst_objects_in_overflowed_buckets,
               worst_objects_in_overflowed_buckets * 100.0 / total_object_count);
    }
}

int main()
{
    srandom(time(NULL));
    unsigned i;
    for(i = 0; i < 16; i++) {
        simulate_repeatedly();
    }
    return 0;
}
