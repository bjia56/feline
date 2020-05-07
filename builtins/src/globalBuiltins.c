#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#include "globalBuiltins.h"

void STRIN_from_cstring(struct STRIN* x, const char* c) {
    x->length = strlen(c);
    free(x->contents);
    x->contents = (char*) malloc(sizeof(char) * (x->length + 1));
    strncpy(x->contents, c, x->length);
    x->contents[x->length] = '\0';
}

void STRIN_CONS(struct STRIN* x) {
    x->contents = NULL;
    x->length = 0;
}

void STRIN_DES(struct STRIN* x) {
    free(x->contents);
    x->contents = 0;
    x->length = 0;
}

struct STRIN* STRIN_CONCAT(struct STRIN* x, struct STRIN* y) {
    struct STRIN* result = (struct STRIN*) malloc(sizeof(struct STRIN));
    result->length = x->length + y->length;
    result->contents = (char*) malloc(sizeof(char) * (result->length + 1));
    strncpy(result->contents, x->contents, x->length);
    strncpy(result->contents + x->length, y->contents, y->length);
    result->contents[result->length] = '\0';
    return result;
}

struct STRIN* ITOA(int x) {
    const size_t MAX_INT_STRLEN = 20;
    struct STRIN* result = (struct STRIN*) malloc(sizeof(struct STRIN));
    result->contents = (char*) malloc(sizeof(char) * (MAX_INT_STRLEN + 1));
    sprintf(result->contents, "%d", x);
    result->length = strlen(result->contents);
    return result;
}
