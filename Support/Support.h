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

bool registerParser(Handle handle, char* parserName, void* parser);

bool unregisterParser(Handle handle, char* parserName);

bool parse(Handle handle, char* path, char* parser);

bool trainTagger(Handle handle, float smoothingFactor);

bool tag(Handle handle, char** words, size_t len, TagId* result);

bool describeTag(Handle handle, TagId tag, char** result, size_t* len);

bool saveTagger(Handle handle, char* path);

bool loadTagger(Handle handle, char* path);

bool saveSentences(Handle handle, char* path);

bool loadSentences(Handle handle, char* path);

bool saveEncoder(Handle handle, char* path);

bool loadEncoder(Handle handle, char* path);

bool saveTreeBuilder(Handle handle, char* path);

bool loadTreeBuilder(Handle handle, char* path);

bool buildDependencyTree(Handle handle, TagId* tags, size_t len, TagId* result);

bool describeRel(Handle handle, TagId tag, char** result, size_t* len);

void release(void* p);

#ifdef __cplusplus
}
#endif

#endif /* SUPPORT_C_INTERFACE */
