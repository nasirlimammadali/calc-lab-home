// Nasirli Mammadali 251ADB068
// https://github.com/nasirilimammadali/calc-lab-home
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources: https://en.cppreference.com/w/c
// AI assistance: ChatGPT helped with parser design, error handling,
// and file processing. See the conversation history for prompts.

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <dirent.h>
#include <sys/stat.h>

typedef struct {
    long long i;
    double f;
    int is_float;
} Value;

static const char *src;
static size_t pos, len, error_pos;
static int failed;

static Value integer(long long n)
{
    Value v = {n, (double)n, 0};
    return v;
}

static Value floating(double n)
{
    Value v = {0, n, 1};
    return v;
}

static double number(Value v)
{
    return v.is_float ? v.f : (double)v.i;
}

static void error_at(size_t p)
{
    if (!failed) {
        failed = 1;
        error_pos = p;
    }
}

static void spaces(void)
{
    while (pos < len &&
           (src[pos] == ' ' || src[pos] == '\t' ||
            src[pos] == '\r'))
        pos++;
}

static Value expression(void);

static Value primary(void)
{
    spaces();

    if (pos >= len) {
        error_at(len + 1);
        return integer(0);
    }

    if (src[pos] == '(') {
        pos++;
        Value v = expression();
        spaces();

        if (!failed) {
            if (pos >= len)
                error_at(len + 1);
            else if (src[pos] != ')')
                error_at(pos + 1);
            else
                pos++;
        }

        return v;
    }

    size_t start = pos;

    if (!isdigit((unsigned char)src[pos])) {
        error_at(pos + 1);
        return integer(0);
    }

    while (pos < len && isdigit((unsigned char)src[pos]))
        pos++;

    int is_float = 0;

    if (pos + 1 < len && src[pos] == '.' &&
        isdigit((unsigned char)src[pos + 1])) {
        is_float = 1;
        pos++;

        while (pos < len && isdigit((unsigned char)src[pos]))
            pos++;
    }

    size_t n = pos - start;
    char *buf = malloc(n + 1);

    if (!buf) {
        error_at(start + 1);
        return integer(0);
    }

    memcpy(buf, src + start, n);
    buf[n] = '\0';

    errno = 0;
    char *endptr = NULL;
    Value v;

    if (is_float) {
        double x = strtod(buf, &endptr);

        if (errno == ERANGE || !isfinite(x))
            error_at(start + 1);

        v = floating(x);
    } else {
        long long x = strtoll(buf, &endptr, 10);

        if (errno == ERANGE)
            error_at(start + 1);

        v = integer(x);
    }

    free(buf);
    return v;
}

static Value power(void)
{
    Value a = primary();
    spaces();

    if (!failed && pos + 1 < len &&
        src[pos] == '*' && src[pos + 1] == '*') {
        size_t op = pos;
        pos += 2;
        spaces();

        size_t exp_start = pos;
        Value b = power();

        if (failed)
            return integer(0);

        if (!b.is_float && b.i < 0) {
            error_at(exp_start + 1);
            return integer(0);
        }

        double r = pow(number(a), number(b));

        if (!isfinite(r)) {
            error_at(op + 1);
            return integer(0);
        }

        if (!a.is_float && !b.is_float) {
            if (b.i > 63) {
                error_at(op + 1);
                return integer(0);
            }

            long long result = 1;
            long long base = a.i;
            long long exponent = b.i;

            while (exponent > 0) {
                if (exponent & 1)
                    result *= base;

                exponent >>= 1;

                if (exponent)
                    base *= base;
            }

            return integer(result);
        }

        return floating(r);
    }

    return a;
}

static Value term(void)
{
    Value a = power();

    while (!failed) {
        spaces();

        if (pos >= len ||
            (src[pos] != '*' && src[pos] != '/'))
            break;

        char op = src[pos++];
spaces();
size_t right_start = pos;
Value b = power();

        if (failed)
            return integer(0);

        if (op == '/' && number(b) == 0.0) {
            error_at(right_start + 1);
            return integer(0);
        }

        if (!a.is_float && !b.is_float) {
            if (op == '*')
                a = integer(a.i * b.i);
            else
                a = integer(a.i / b.i);
        } else {
            double r = op == '*'
                ? number(a) * number(b)
                : number(a) / number(b);

            if (!isfinite(r)) {
                error_at(right_start + 1);
                return integer(0);
            }

            a = floating(r);
        }
    }

    return a;
}

static Value expression(void)
{
    Value a = term();

    while (!failed) {
        spaces();

        if (pos >= len ||
            (src[pos] != '+' && src[pos] != '-'))
            break;

        char op = src[pos++];
        Value b = term();

        if (failed)
            return integer(0);

        if (!a.is_float && !b.is_float) {
            a = integer(op == '+' ? a.i + b.i : a.i - b.i);
        } else {
            double r = op == '+'
                ? number(a) + number(b)
                : number(a) - number(b);

            if (!isfinite(r)) {
                error_at(pos);
                return integer(0);
            }

            a = floating(r);
        }
    }

    return a;
}

static int evaluate(const char *text, size_t n,
                    size_t start, Value *result, size_t *err)
{
    src = text;
    pos = 0;
    len = n;
    failed = 0;
    error_pos = 1;

    spaces();

    if (pos == len || src[pos] == '#')
        return 0;

    Value v = expression();
    spaces();

    if (!failed && pos < len)
        error_at(pos + 1);

    if (failed) {
        *err = start + error_pos;
        return -1;
    }

    *result = v;
    return 1;
}

static int read_file(const char *path, char **data, size_t *n)
{
    FILE *f = fopen(path, "rb");

    if (!f)
        return 0;

    size_t cap = 8192;
    size_t used = 0;
    char *buf = malloc(cap);

    if (!buf) {
        fclose(f);
        return 0;
    }

    int ch;

    while ((ch = fgetc(f)) != EOF) {
        if (used + 1 >= cap) {
            if (cap > SIZE_MAX / 2) {
                free(buf);
                fclose(f);
                return 0;
            }

            cap *= 2;
            char *next = realloc(buf, cap);

            if (!next) {
                free(buf);
                fclose(f);
                return 0;
            }

            buf = next;
        }

        buf[used++] = (char)ch;
    }

    if (ferror(f)) {
        free(buf);
        fclose(f);
        return 0;
    }

    fclose(f);
    buf[used] = '\0';
    *data = buf;
    *n = used;
    return 1;
}

static char *join_path(const char *a, const char *b)
{
    size_t n = strlen(a) + strlen(b) + 2;
    char *s = malloc(n);

    if (s)
        snprintf(s, n, "%s/%s", a, b);

    return s;
}

static char *copy_string(const char *s)
{
    size_t n = strlen(s) + 1;
    char *p = malloc(n);

    if (p)
        memcpy(p, s, n);

    return p;
}

static char *strip_txt(const char *s)
{
    char *p = copy_string(s);

    if (!p)
        return NULL;

    size_t n = strlen(p);

    if (n >= 4 && strcmp(p + n - 4, ".txt") == 0)
        p[n - 4] = '\0';

    return p;
}

static int make_dir(const char *path)
{
    struct stat st;

    if (stat(path, &st) == 0)
        return S_ISDIR(st.st_mode);

    return mkdir(path, 0777) == 0;
}

static int process_file(const char *path, const char *outdir,
                        const char *base)
{
    char *data = NULL;
    size_t n = 0;

    if (!read_file(path, &data, &n)) {
        fprintf(stderr, "Cannot read input file: %s\n", path);
        return 1;
    }

    size_t name_len = strlen(base) + 40;
    char *name = malloc(name_len);

    if (!name) {
        free(data);
        return 1;
    }

    snprintf(name, name_len, "%s_mammadali_nasirli_251ADB068.txt",
             base);

    char *outpath = join_path(outdir, name);
    free(name);

    if (!outpath) {
        free(data);
        return 1;
    }

    FILE *tmp = tmpfile();

    if (!tmp) {
        free(outpath);
        free(data);
        return 1;
    }

    size_t start = 0;
    size_t err = 0;

    while (start < n) {
        size_t end = start;

        while (end < n && data[end] != '\n')
            end++;

        Value v;
        size_t error = 0;

        int r = evaluate(data + start, end - start,
                         start, &v, &error);

        if (r < 0) {
            err = error;
            break;
        }

        if (r > 0) {
            if (v.is_float)
                fprintf(tmp, "%.15g\n", v.f);
            else
                fprintf(tmp, "%lld\n", v.i);
        }

        start = end + (end < n ? 1 : 0);
    }

    FILE *out = fopen(outpath, "w");

    if (!out) {
        fclose(tmp);
        free(outpath);
        free(data);
        return 1;
    }

    if (err) {
        fprintf(out, "ERROR:%zu\n", err);
    } else {
        rewind(tmp);
        int ch;

        while ((ch = fgetc(tmp)) != EOF)
            fputc(ch, out);
    }

    fclose(out);
    fclose(tmp);
    free(outpath);
    free(data);
    return 0;
}

static int process_directory(const char *dir, const char *outdir)
{
    DIR *d = opendir(dir);

    if (!d) {
        fprintf(stderr, "Cannot open directory: %s\n", dir);
        return 1;
    }

    int status = 0;
    struct dirent *entry;

    while ((entry = readdir(d)) != NULL) {
        size_t n = strlen(entry->d_name);

        if (n < 4 ||
            strcmp(entry->d_name + n - 4, ".txt") != 0)
            continue;

        char *path = join_path(dir, entry->d_name);
        char *base = strip_txt(entry->d_name);

        if (!path || !base) {
            free(path);
            free(base);
            status = 1;
            break;
        }

        struct stat st;

        if (stat(path, &st) == 0 && S_ISREG(st.st_mode)) {
            if (process_file(path, outdir, base))
                status = 1;
        }

        free(path);
        free(base);
    }

    closedir(d);
    return status;
}

int main(int argc, char **argv)
{
    const char *dir = NULL;
    const char *outdir = NULL;
    const char *input = NULL;

    for (int i = 1; i < argc; i++) {
        if ((!strcmp(argv[i], "-d") ||
             !strcmp(argv[i], "--dir")) && i + 1 < argc) {
            dir = argv[++i];
        } else if ((!strcmp(argv[i], "-o") ||
                    !strcmp(argv[i], "--output-dir")) &&
                   i + 1 < argc) {
            outdir = argv[++i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            return 1;
        } else if (!input) {
            input = argv[i];
        } else {
            fprintf(stderr, "Too many input arguments\n");
            return 1;
        }
    }

    if ((dir && input) || (!dir && !input)) {
        fprintf(stderr,
                "Usage: calc [-d DIR | input.txt] [-o OUTDIR]\n");
        return 1;
    }

    const char *source = dir ? dir : input;
    struct stat st;

    if (stat(source, &st) != 0 ||
        (dir ? !S_ISDIR(st.st_mode) : !S_ISREG(st.st_mode))) {
        fprintf(stderr, "Invalid input: %s\n", source);
        return 1;
    }

    char *default_out = NULL;

    if (!outdir) {
        const char *slash = strrchr(source, '/');
        const char *base = slash ? slash + 1 : source;
        char *stem = dir ? copy_string(base) : strip_txt(base);

        if (!stem)
            return 1;

        size_t n = strlen(source) + strlen(stem) + 40;
        default_out = malloc(n);

        if (!default_out) {
            free(stem);
            return 1;
        }

        if (slash) {
            size_t prefix = (size_t)(slash - source);

            snprintf(default_out, n, "%.*s/%s_mammadali_nasirli_251ADB068",
                     (int)prefix, source, stem);
        } else {
            snprintf(default_out, n, "%s_mammadali_nasirli_251ADB068",
                     stem);
        }

        free(stem);
        outdir = default_out;
    }

    if (!make_dir(outdir)) {
        fprintf(stderr, "Cannot create output directory: %s\n", outdir);
        free(default_out);
        return 1;
    }

    int status;

    if (dir) {
        status = process_directory(dir, outdir);
    } else {
        const char *basepath = strrchr(input, '/');
        basepath = basepath ? basepath + 1 : input;

        char *base = strip_txt(basepath);

        if (!base) {
            free(default_out);
            return 1;
        }

        status = process_file(input, outdir, base);
        free(base);
    }

    free(default_out);
    return status;
}