#ifndef SUPPORT_C_INTERFACE
#define SUPPORT_C_INTERFACE

#include <stdbool.h>
#include <stddef.h>

#include "Types.h"

#ifdef __cplusplus
extern "C" {
#endif

bool parse(char* path, char* parser);

bool trainTagger(float smoothingFactor);

bool tag(char** words, size_t len, TagId* result);

bool tag(char** words, size_t len, TagId* result);

bool getCompoundPOSTag(TagId tag, TagId* result, size_t* len);

bool describeTag(TagId tag, char** result, size_t* len);

bool saveTagger(char* path);

bool loadTagger(char* path);

bool saveSentences(char* path);

bool loadSentences(char* path);

bool saveEncoder(char* path);

bool loadEncoder(char* path);

bool saveTreeBuilder(char* path);

bool loadTreeBuilder(char* path);

bool buildDependencyTree(TagId* tags, size_t len, TagId* result);

bool describeRel(TagId tag, char** result, size_t* len);

#ifdef __cplusplus
}
#endif

#endif /* SUPPORT_C_INTERFACE */
