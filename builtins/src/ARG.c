#include "stdlib.h"
#include "string.h"

#include "globalBuiltins.h"

int global_argc_index = 0;
int global_argc = 0;
char** global_argv = NULL;

void main_load_args(int argc, char** argv) {
    global_argc_index = 0;
    global_argc = argc;
    global_argv = argv;
}

struct STRIN* NEXTARG() {
    if (global_argc_index == global_argc) {
        return NULL;
    }

    size_t arglen = strlen(global_argv[global_argc_index]);
    struct STRIN* result = (struct STRIN*) malloc(sizeof(struct STRIN));
    memset(result, 0, sizeof(struct STRIN));
    result->contents = (char*) malloc(sizeof(char) * (arglen + 1));
    memset(result->contents, 0, sizeof(char) * (arglen + 1));
    strncpy(result->contents, global_argv[global_argc_index], arglen);
    result->contents[arglen] = '\0';
    result->length = arglen;

    global_argc_index++;

    return result;
}
