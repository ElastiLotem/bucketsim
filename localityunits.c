#include <infra/arithmetic_api.h>
#include <infra/dstruct/hash.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdbool.h>
#include <assert.h>
#include <inttypes.h>

#define TOTAL_SIZE    (1ULL << 40)
#define DIRECT_SIZE   (TOTAL_SIZE / 20)
#define BUCKET_COUNT  (DIRECT_SIZE / BUCKET_SIZE)
#define BUCKET_SIZE   (4ULL << 10)
/* #define H2OENTRY_SIZE (32) */
/* #define H2O_OVERHEAD_PER_BUCKET  ((TOTAL_SIZE / DIRECT_SIZE) * H2OENTRY_SIZE / 4096) */
#define OVERFLOW_THRESHOLD ((4ULL << 10) - 64) // - H2O_OVERHEAD_PER_BUCKET)
#define COMPRESSION_RATIO 0.5

#define MAX_OBJS_PER_BUCKET  20

#define LOCALITY_UNIT_SIZE(ptr_count)      3200 // (32 + ((ptr_count) * 21))

#define TOTAL_OBJECT_COUNT(locality_size)  ((TOTAL_SIZE - DIRECT_SIZE) / (locality_size))

#define REPEAT_COUNT  (16LU)

struct result {
    uint64_t max;
    uint64_t max_count;         /* TODO: remove? */
    uint64_t overflows;
    uint64_t overflowed_bytes;
    uint64_t bucket_utilization_in_objs[MAX_OBJS_PER_BUCKET+1];
    uint64_t objects_in_overflowed_buckets;
};
static inline struct result result_init(uint64_t bucket_count)
{
    return (struct result){ 0, 0, 0, 0, {bucket_count}, 0 };
}

static inline struct result simulate(uint64_t compressed_locality_size, uint64_t locality_ptrs_size)
{
    struct bucket
    {
        uint64_t obj_count;
        uint64_t byte_count;
    };
    static struct bucket buckets[BUCKET_COUNT]; // Not thread safe!
    memset(buckets, 0, sizeof buckets);
    struct result res = result_init(BUCKET_COUNT);
    uint64_t iter = 0;
    for(iter = 0; iter < TOTAL_OBJECT_COUNT(compressed_locality_size); iter++) {
        uint64_t murmur = hash_murmur_hash64(PS(iter), 0);
        uint64_t index = murmur /* random() */ % BUCKET_COUNT;
        bool old_overflow = buckets[index].byte_count > OVERFLOW_THRESHOLD;
        buckets[index].byte_count += locality_ptrs_size;
        assert(buckets[index].obj_count + 1 < ARRAY_LEN(res.bucket_utilization_in_objs));
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
    uint64_t i;
    printf("  --------------------------\n");
    for(i=0; i < MAX_OBJS_PER_BUCKET+1; i++) {
        printf("  %8" PRIu64, i);
    }
    printf("\n");
    for(i=0; i < MAX_OBJS_PER_BUCKET+1; i++) {
        printf("  %8" PRIu64, res.bucket_utilization_in_objs[i]);
    }
    printf("\n");
    for(i=0; i < MAX_OBJS_PER_BUCKET+1; i++) {
        uint64_t prev = i > 0 ? res.bucket_utilization_in_objs[i-1] : 0;
        if(0 == prev) {
            printf("  %8s", "");
        } else {
            printf("  %1.6f", (double)res.bucket_utilization_in_objs[i] / prev);
        }
    }
    printf("\n");
    for(i=0; i < MAX_OBJS_PER_BUCKET+1; i++) {
        if (0 == res.bucket_utilization_in_objs[i]) {
            printf("  %8u", 0);
        } else {
            printf("  %1.6f", (double)res.bucket_utilization_in_objs[i] / BUCKET_COUNT);
        }
    }
    printf("\n");
    printf("  %8s", "");
    const uint64_t nonempty_bucket_count = BUCKET_COUNT - res.bucket_utilization_in_objs[0];
    for(i=1; i < MAX_OBJS_PER_BUCKET+1; i++) {
        printf("  %1.6f", (double)res.bucket_utilization_in_objs[i] / nonempty_bucket_count);
    }
    printf("\n  --------------------------\n");
}

static inline void simulate_repeatedly(void)
{
    printf("Total size = %llu MB, Bucket count = %llu, bucket size = %lluKB, compression_ratio = %g\n",
           TOTAL_SIZE >> 20, BUCKET_COUNT, BUCKET_SIZE >> 10, COMPRESSION_RATIO);
    uint64_t raw_locality_size;
    for(raw_locality_size = (1ULL<<19); raw_locality_size <= (1ULL<<19); raw_locality_size *= 2) {
        uint64_t compressed_locality_size = raw_locality_size * COMPRESSION_RATIO;
//        uint64_t ptr_count = raw_locality_size >> 12;
        uint64_t locality_ptrs_size = LOCALITY_UNIT_SIZE(ptr_count);

        printf("Object size: %"PRIu64"\n", locality_ptrs_size);

        double total_overflowed_bytes = 0;
        double total_max = 0, total_overflows = 0, total_objects_in_overflowed_buckets = 0;
        uint64_t worst_max = 0, worst_overflows = 0, worst_objects_in_overflowed_buckets = 0;
        uint64_t i;
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
        uint64_t total_object_count = TOTAL_OBJECT_COUNT(compressed_locality_size);
        uint64_t total_byte_count = (uint64_t)total_object_count * locality_ptrs_size;
        printf("Locality size: %7"PRIu64", Ptrs size: %4"PRIu64", Object count: %6"PRIu64", Avg bucket util.: %7g, %"PRIu64" runs:\n"
               "    Avg Max bucket util.: %7g.   Avg Overflows = %8g Objects in overflowed = %g (%g%%)\n"
               "  Worst Max bucket util.: %7"PRIu64". Worst Overflows = %8"PRIu64" Worst Objects in overflowed = %"PRIu64" (%g%%)\n"
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
    uint64_t i;
    for(i = 0; i < 16; i++) {
        simulate_repeatedly();
    }
    return 0;
}
