#include "stdio.h"
#include "stdlib.h"

#include "globalBuiltins.h"

void MEOW(struct STRIN* x) {
    printf("%s\n", x->contents);
}

struct STRIN* NOM() {
    char *line = NULL;
    size_t len = 0;
    size_t nread = getline(&line, &len, stdin);

    if (line[nread - 1] == '\n') {
        line[nread - 1] = '\0'; // trim off delimiter
    }

    struct STRIN* result = (struct STRIN*)malloc(sizeof(struct STRIN));
    result->length = len;
    result->contents = line;

    return result;
}
