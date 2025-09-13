#include "Support.h"

#include "Engine/Engine.h"
#include "CoNLLU/Parser.h"

#include "spdlog/spdlog.h"

CoNLLUParser conlluParser;

Handle init()
{
    Engine* pEngine = new Engine();

    pEngine->registerParser("CoNLLU", conlluParser);

    return pEngine;
}

void clear(Handle handle)
{
    Engine* pEngine = (Engine*)handle;

    if (pEngine)
    {
        delete pEngine;
        return;
    }

    spdlog::error("Engine handle in null");
}

bool parse(Handle handle, char* path, char* parser)
{
    Engine* pEngine = (Engine*)handle;

    if (pEngine)
    {
        return pEngine->parse(path, parser);
    }

    spdlog::error("Engine handle in null");
    return false;
}

bool save(Handle handle, char* path)
{
    Engine* pEngine = (Engine*)handle;

    if (pEngine)
    {
        return pEngine->save(path);
    }

    spdlog::error("Engine handle in null");
    return false;
}

bool registerParser(Handle handle, char* parserName, void* parser)
{
    if (!parser)
    {
        spdlog::error("Parser handle in null");
        return false;
    }

    Parser* pParser = (Parser*)parser;

    Engine* pEngine = (Engine*)handle;

    if (pEngine)
    {
        return pEngine->registerParser(parserName, *pParser);
    }

    spdlog::error("Engine handle in null");
    return false;
}

bool unregisterParser(Handle handle, char* parserName)
{
    Engine* pEngine = (Engine*)handle;

    if (pEngine)
    {
        return pEngine->unregisterParser(parserName);
    }

    spdlog::error("Engine handle in null");
    return false;
}

bool tag(Handle handle, char** words, size_t len, TagId* result)
{
    if (!result)
    {
        spdlog::error("Result is null");
        return false;
    }

    Engine* pEngine = (Engine*)handle;

    if (pEngine)
    {
        std::vector<std::string> v(len);
        std::copy(words, words + len, v.begin());

        std::optional<Tags> res = pEngine->tag(v);

        if (!res)
        {
            return false;
        }

        std::copy(res->begin(), res->end(), result);

        return true;
    }

    spdlog::error("Engine handle in null");
    return false;
}

const size_t MAX_NAME_LENGTH = 16;
const size_t MAX_BUFFER_SIZE = (2 * MAX_FEATURES_PER_WORD + 1) * MAX_NAME_LENGTH;

bool describeTag(Handle handle, TagId tag, char** result, size_t* len)
{
    if (!result || !len)
    {
        spdlog::error("Result is null");
        return false;
    }

    Engine* pEngine = (Engine*)handle;

    if (pEngine)
    {
        const auto description = pEngine->describePOSTag(tag);

        if (!description)
        {
            spdlog::error("failed to get description for tag {}", tag);
            return false;
        }

        *len = 1 + 2 * description->features.size();

        static char buffer[MAX_BUFFER_SIZE];

        result[0] = buffer;
        char* pos = stpncpy(result[0], description->POS.c_str(), MAX_NAME_LENGTH);

        for (size_t f = 0; f < std::min(description->features.size(), size_t(MAX_FEATURES_PER_WORD)); ++f)
        {
            result[2 * f + 1] = pos + 1;
            pos = strncpy(result[2 * f + 1], description->features[f].first.c_str(), MAX_NAME_LENGTH);
            result[2 * f + 2] = pos + 1;
            pos = strncpy(result[2 * f + 2], description->features[f].second.c_str(), MAX_NAME_LENGTH);
        }

        return true;
    }

    spdlog::error("Engine handle in null");
    return false;
}


void release(void* p)
{
    if (p)
    {
        free(p);
    }
}