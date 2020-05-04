#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#include "globalBuiltins.h"

void STRIN_from_cstring(struct STRIN* x, const char* c) {
    x->length = strlen(c);
    free(x->contents);
    x->contents = (char*) malloc(sizeof(char) * (x->length) + 1);
    strncpy(x->contents, c, x->length);
    x->contents[x->length] = '\0';
}

void STRIN_CONS(struct STRIN* x) {
    x->contents = NULL;
    x->length = 0;
}

void STRIN_DES(struct STRIN* x) {
    free(x->contents);
}
