#ifndef FIO_H
#define FIO_H

#include "stdio.h"

struct FYL {
    char* path;
    FILE* handle;
    char eof;
};

#endif
