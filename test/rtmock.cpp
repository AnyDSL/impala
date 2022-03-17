#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <setjmp.h>
#include <cstring>


#ifdef __cplusplus
extern "C" {
#endif

void print_char(char c) {
   printf("%c\n", (int)c);
}
void print_int(int i) {
   printf("%d\n", i);
}
void print_f64(double d) {
   printf("%.9f\n", d);
}

void print_piece_mask(unsigned long long* a) {
    int i;
    for (i = 0; i < 12; ++i)
        printf("%lli ", a[i]);
    printf("\n");
}

void print_piece_def(char* a) {
    int i;
    for (i = 0; i < 40; ++i)
        printf("%d ", a[i]);
    printf("\n");
}

// header for mandelbrot bitmap
void print_header(int w, int h) {
   printf("P4\n%d %d\n",w,h);
}

void put_u8(unsigned char ui) {
   putc(ui, stdout);
}

#if _POSIX_VERSION >= 200112L || _XOPEN_SOURCE >= 600
void* anydsl_aligned_malloc(size_t size, size_t alignment) {
    void* p = nullptr;
    posix_memalign(&p, alignment, size);
    return p;
}
void anydsl_aligned_free(void* ptr) { free(ptr); }
#elif _ISOC11_SOURCE
void* anydsl_aligned_malloc(size_t size, size_t alignment) { return ::aligned_alloc(alignment, size); }
void anydsl_aligned_free(void* ptr) { ::free(ptr); }
#elif defined(_WIN32) || defined(__CYGWIN__)
#include <malloc.h>

void* anydsl_aligned_malloc(size_t size, size_t alignment) { return ::_aligned_malloc(size, alignment); }
void anydsl_aligned_free(void* ptr) { ::_aligned_free(ptr); }
#else
#error "There is no way to allocate aligned memory on this system"
#endif
void* anydsl_alloc(int32_t dev, int64_t size) {
    // TODO: check whether aligned memory is actually necessary
    return anydsl_aligned_malloc(size, 64);
}

// meteor printing
void print_meteor_scnt(int cnt) {
   printf("%d solutions found\n\n", cnt);
}

void print_meteor_lines(char a0, char a1, char a2, char a3, char a4, char a5, char a6, char a7, char a8, char a9) {
   printf("%c %c %c %c %c \n %c %c %c %c %c \n", a0+'0', a1+'0', a2+'0', a3+'0', a4+'0', a5+'0', a6+'0', a7+'0', a8+'0', a9+'0');
}

void write(const char* line, size_t size) {
    fwrite(line, size, 1, stdout);
}

void print(const char* s) {
    printf("%s", s);
}

void printn(const char* s, int n) {
    fwrite(s, 1, n, stdout);
}

void println(const char* s) {
    printf("%s\n", s);
}

FILE* get_stdin() {
    return stdin;
}

void print_num_matches(const char* s, int count) {
    printf("%s %d\n", s, count);
}

void print_digits(unsigned long long i) {
    printf("\t:%llu\n", i);
}

void saveppm(int w, int h, unsigned char *img) {
    printf("P6\n");
    printf("%d %d\n", w, h);
    printf("255\n");
    fwrite(img, w * h * 3, 1, stdout);
}

// reverse
int read(char* s, int bufsize) {
    return (fgets(s, bufsize, stdin)) ? 1 : 0;
}

void* impala_realloc(void* ptr, int size) {
    return realloc(ptr, size);
}

void impala_memmove(char* dest, const char* src, int size) {
    std::memmove(dest, src, size);
}

// pe_known
int forty_two() {
    return 42;
}

// helper for first-class continuations
int64_t jmpbuf_size() {
    return sizeof(jmp_buf);
}

#ifdef __cplusplus
}
#endif


// polyfill of non-standard drand48()
#ifdef _MSC_VER

#include <random>
static std::mt19937_64 std_gen64;
static std::uniform_real_distribution<double> std_dist64(0., 1.);
extern "C" void srand48(int64_t seed) { std_gen64.seed(seed); }
extern "C" double drand48() { return std_dist64(std_gen64); }

static std::uniform_int_distribution<int64_t> std_disti64(-(1 << 31), 1 << 31);
extern "C" int64_t mrand48() { return std_disti64(std_gen64); }

#endif // _MSC_VER
