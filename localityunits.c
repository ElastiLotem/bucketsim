#include <infra/arithmetic_api.h>
#include <infra/dstruct/hash.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdbool.h>

#define TOTAL_SIZE    (1ULL << 38)
#define DIRECT_SIZE   (TOTAL_SIZE / 20)
#define BUCKET_COUNT  (DIRECT_SIZE / BUCKET_SIZE)
#define BUCKET_SIZE   (4ULL << 10)
#define H2OENTRY_SIZE (32)
#define H2O_OVERHEAD_PER_BUCKET  ((TOTAL_SIZE / DIRECT_SIZE) * H2OENTRY_SIZE / 4096)
#define OVERFLOW_THRESHOLD ((4ULL << 10) - 64 - H2O_OVERHEAD_PER_BUCKET)
#define COMPRESSION_RATIO 0.5

#define MAX_OBJS_PER_BUCKET  20

#define LOCALITY_UNIT_SIZE(ptr_count)      (32 + ((ptr_count) * 21))

#define TOTAL_OBJECT_COUNT(locality_size)  ((TOTAL_SIZE - DIRECT_SIZE) / (locality_size))

#define REPEAT_COUNT  (16)

struct result {
    unsigned max;
    unsigned max_count;         /* TODO: remove? */
    unsigned overflows;
    unsigned overflowed_bytes;
    unsigned bucket_utilization_in_objs[MAX_OBJS_PER_BUCKET+1];
    unsigned objects_in_overflowed_buckets;
};
static inline struct result result_init(unsigned bucket_count)
{
    return (struct result){ 0, 0, 0, 0, {bucket_count}, 0 };
}

static inline struct result simulate(unsigned compressed_locality_size, unsigned locality_ptrs_size)
{
    struct bucket
    {
        unsigned obj_count;
        unsigned byte_count;
    };
    static struct bucket buckets[BUCKET_COUNT]; // Not thread safe!
    memset(buckets, 0, sizeof buckets);
    struct result res = result_init(BUCKET_COUNT);
    unsigned iter = 0;
    for(iter = 0; iter < TOTAL_OBJECT_COUNT(compressed_locality_size); iter++) {
        unsigned murmur = hash__murmur_hash_64a(PS(iter), 0);
        unsigned index = murmur /* random() */ % BUCKET_COUNT;
        bool old_overflow = buckets[index].byte_count > OVERFLOW_THRESHOLD;
        buckets[index].byte_count += locality_ptrs_size;
        res.bucket_utilization_in_objs[buckets[index].obj_count]--;
        buckets[index].obj_count++;
        res.bucket_utilization_in_objs[buckets[index].obj_count]++;
        bool new_overflow = buckets[index].byte_count > OVERFLOW_THRESHOLD;
        if(new_overflow) {
            if(old_overflow) {
                res.objects_in_overflowed_buckets++;
                res.overflowed_bytes += locality_ptrs_size;
            } else {
                res.objects_in_overflowed_buckets += buckets[index].obj_count;
                res.overflowed_bytes += buckets[index].byte_count - OVERFLOW_THRESHOLD;
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

static void print_histogram(struct result res)
{
    unsigned i;
    printf("  --------------------------\n");
    for(i=0; i < MAX_OBJS_PER_BUCKET+1; i++) {
        printf("  %8d", i);
    }
    printf("\n");
    for(i=0; i < MAX_OBJS_PER_BUCKET+1; i++) {
        printf("  %8d", res.bucket_utilization_in_objs[i]);
    }
    printf("\n");
    for(i=0; i < MAX_OBJS_PER_BUCKET+1; i++) {
        unsigned prev = i > 0 ? res.bucket_utilization_in_objs[i-1] : 0;
        if(0 == prev) {
            printf("  %8s", "");
        } else {
            printf("  .%07llu", 10000000ULL * res.bucket_utilization_in_objs[i] / prev);
        }
    }
    printf("\n  --------------------------\n");
}

static inline void simulate_repeatedly(void)
{
    printf("Total size = %llu MB, Bucket count = %llu, bucket size = %lluKB, compression_ratio = %g\n",
           TOTAL_SIZE >> 20, BUCKET_COUNT, BUCKET_SIZE >> 10, COMPRESSION_RATIO);
    unsigned raw_locality_size;
    for(raw_locality_size = (1ULL<<19); raw_locality_size <= (1ULL<<19); raw_locality_size *= 2) {
        unsigned compressed_locality_size = raw_locality_size * COMPRESSION_RATIO;
        unsigned ptr_count = raw_locality_size >> 12;
        unsigned locality_ptrs_size = LOCALITY_UNIT_SIZE(ptr_count);

        printf("Object size: %u\n", locality_ptrs_size);

        double total_overflowed_bytes = 0;
        double total_max = 0, total_overflows = 0, total_objects_in_overflowed_buckets = 0;
        unsigned worst_max = 0, worst_overflows = 0, worst_objects_in_overflowed_buckets = 0;
        unsigned i;
        struct result worst_res = result_init(0);
        for(i = 0; i < REPEAT_COUNT; i++) {
            struct result res = simulate(compressed_locality_size, locality_ptrs_size);
            total_max += res.max;
            if(res.max > worst_max) {
                worst_res = res;
                worst_max = res.max;
            }
            total_overflows += res.overflows;
            worst_overflows = MAX(worst_overflows, res.overflows);
            total_objects_in_overflowed_buckets += res.objects_in_overflowed_buckets;
            worst_objects_in_overflowed_buckets =
                MAX(worst_objects_in_overflowed_buckets, res.objects_in_overflowed_buckets);
            total_overflowed_bytes += res.overflowed_bytes;
        }
        print_histogram(worst_res);
        unsigned total_object_count = TOTAL_OBJECT_COUNT(compressed_locality_size);
        unsigned long total_byte_count = (unsigned long)total_object_count * locality_ptrs_size;
        printf("Locality size: %7u, Ptrs size: %4u, Object count: %6u, Avg bucket util.: %7g, %u runs:\n"
               "    Avg Max bucket util.: %7g.   Avg Overflows = %8g Objects in overflowed = %g (%g%%)\n"
               "  Worst Max bucket util.: %7u. Worst Overflows = %8u Worst Objects in overflowed = %u (%g%%)\n"
               "    Avg overflowed bytes: %7g (%g%%)\n",
               raw_locality_size, locality_ptrs_size, total_object_count,
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
               worst_objects_in_overflowed_buckets * 100.0 / total_object_count,
               total_overflowed_bytes / REPEAT_COUNT,
               total_overflowed_bytes / REPEAT_COUNT * 100.0 / total_byte_count);
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
