#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#include "globalBuiltins.h"

char isStdinEof = 0;

void MEOW(struct STRIN* x) {
    printf("%s\n", x->contents);
}

struct STRIN* NOM() {
    if (isStdinEof) {
        return NULL;
    }

    char *line = NULL;
    size_t len = 0;
    size_t nread = getline(&line, &len, stdin);

    if (nread == -1) {
        isStdinEof = 1;
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

char FIN() {
    return isStdinEof;
}
