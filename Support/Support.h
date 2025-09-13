#ifndef SUPPORT_C_INTERFACE
#define SUPPORT_C_INTERFACE

#include <stdbool.h>
#include <stddef.h>

#include "Types.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef void* Handle;

Handle init();

void clear(Handle handle);

bool load(Handle handle, char* path, char* parser);

bool save(Handle handle, char* path);

bool registerParser(Handle handle, char* parserName, void* parser);

bool unregisterParser(Handle handle, char* parserName);

bool tag(Handle handle, char** words, size_t len, TagId* result);

bool describeTag(Handle handle, TagId tag, char** result, size_t* len);

void release(void* p);

#ifdef __cplusplus
}
#endif

#endif /* SUPPORT_C_INTERFACE */
