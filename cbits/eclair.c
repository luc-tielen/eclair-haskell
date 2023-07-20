// This file defines weak symbols, so they can be properly linked later on.

#include <stddef.h>
#include <stdint.h>

#define WEAK_SYMBOL __attribute__((weak))

struct program;
struct symbol;

struct program* WEAK_SYMBOL eclair_program_init() { return NULL; }

void WEAK_SYMBOL eclair_program_run(struct program*) {}

void WEAK_SYMBOL eclair_program_destroy(struct program*) {}

struct symbol* WEAK_SYMBOL eclair_decode_string(struct program*, uint32_t string_index) { return NULL; }

uint32_t WEAK_SYMBOL eclair_encode_string(struct program*, uint32_t string_length, const char* string_data) { return 0; }

void WEAK_SYMBOL eclair_add_fact(struct program* program, uint32_t fact_type, uint32_t* fact_data) {}

void WEAK_SYMBOL eclair_add_facts(struct program* program, uint32_t fact_type, uint32_t* fact_data, uint32_t fact_count) {}

uint32_t WEAK_SYMBOL eclair_fact_count(struct program*, uint32_t fact_type) { return 0; }

uint32_t* WEAK_SYMBOL eclair_get_facts(struct program*, uint32_t fact_type) { return NULL; }

void WEAK_SYMBOL eclair_free_buffer(uint32_t* data) {}
