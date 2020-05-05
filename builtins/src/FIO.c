#include "stdio.h"
#include "stdlib.h"

#include "globalBuiltins.h"
#include "FIO.h"

void FYL_CONS(struct FYL* x) {
    // Do nothing
}

void FYL_DES(struct FYL* x) {
    if (x->handle != 0) {
        fclose(x->handle);
    }
}

void FYL_MEOW(struct FYL* x, struct STRIN* y) {
    fprintf(x->handle, "%s\n", y->contents);
}

struct STRIN* FYL_NOM(struct FYL* x) {
    char *line = NULL;
    size_t len = 0;
    size_t nread = getline(&line, &len, x->handle);

    if (line[nread - 1] == '\n') {
        line[nread - 1] = '\0'; // trim off delimiter
    }

    struct STRIN* result = (struct STRIN*)malloc(sizeof(struct STRIN));
    result->length = len;
    result->contents = line;

    return result;
}

struct FYL* OPEN(struct STRIN* path) {
    FILE* handle = fopen(path->contents, "a+");
    if (handle == NULL) {
        return NULL;
    }

    struct FYL* result = (struct FYL*)malloc(sizeof(struct FYL));
    result->handle = handle;
    result->isOpen = 1;

    return result;
}
