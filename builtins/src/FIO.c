#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "sys/stat.h"

#include "globalBuiltins.h"
#include "FIO.h"

void FYL_CONS(struct FYL* x) {
    // Do nothing
}

void FYL_DES(struct FYL* x) {
    if (x->handle != 0) {
        fclose(x->handle);
        free(x->path);
        x->handle = 0;
        x->path = 0;
    }
}

void FYL_MEOW(struct FYL* x, struct STRIN* y) {
    fprintf(x->handle, "%s\n", y->contents);
}

struct STRIN* FYL_NOM(struct FYL* x) {
    if (x->eof) {
        return NULL;
    }

    char *line = NULL;
    size_t len = 0;
    size_t nread = getline(&line, &len, x->handle);

    if (nread == -1) {
        x->eof = 1;
        return NULL;
    }

    if (line[nread - 1] == '\n') {
        line[nread - 1] = '\0'; // trim off delimiter
    }

    struct STRIN* result = (struct STRIN*)malloc(sizeof(struct STRIN));
    memset(result, 0, sizeof(struct STRIN));
    result->length = len;
    result->contents = line;

    return result;
}

int FYL_SYZ(struct FYL* x) {
    struct stat st;
    stat(x->path, &st);
    return st.st_size;
}

char FYL_FIN(struct FYL* x) {
    return x->eof;
}

struct FYL* OPEN(struct STRIN* path) {
    FILE* handle = fopen(path->contents, "a+");
    if (handle == NULL) {
        return NULL;
    }

    char* cpath = (char*)malloc(sizeof(char) * (path->length + 1));
    memset(cpath, 0, sizeof(char) * (path->length + 1));
    strncpy(cpath, path->contents, path->length);
    cpath[path->length] = '\0';

    struct FYL* result = (struct FYL*)malloc(sizeof(struct FYL));
    memset(result, 0, sizeof(struct FYL));
    result->handle = handle;
    result->path = cpath;
    result->eof = 0;

    return result;
}

char IZFYL(struct STRIN* path) {
    struct stat st;
    stat(path->contents, &st);
    return S_ISREG(st.st_mode);
}

char IZDYR(struct STRIN* path) {
    struct stat st;
    stat(path->contents, &st);
    return S_ISDIR(st.st_mode);
}
