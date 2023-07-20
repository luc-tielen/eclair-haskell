// This file defines weak symbols, so they can be properly linked later on.

#include <stddef.h>

struct program;

struct program* __attribute__((weak)) eclair_program_init() {
    return NULL;
}
