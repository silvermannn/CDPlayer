#include "Support.h"

#include "Engine/Engine.h"

#include "spdlog/spdlog.h"

bool parse(char* path, char* parser)
{
    return Engine::singleton().parse(path, parser);
}

bool trainTagger(float smoothingFactor)
{
    return Engine::singleton().trainTagger(smoothingFactor);
}

bool saveSentences(char* path)
{
    return Engine::singleton().saveSentences(path);
}

bool loadSentences(char* path)
{
    return Engine::singleton().loadSentences(path);
}

bool saveEncoder(char* path)
{
    return Engine::singleton().saveEncoder(path);
}

bool loadEncoder(char* path)
{
    return Engine::singleton().loadEncoder(path);
}

bool tag(char** words, size_t len, TagId* result)
{
    if (!result)
    {
        spdlog::error("Result is null");
        return false;
    }

    std::vector<std::string> v(len);
    std::copy(words, words + len, v.begin());

    std::optional<Tags> res = Engine::singleton().tag(v);

    if (!res)
    {
        return false;
    }

    std::copy(res->begin(), res->end(), result);

    return true;
}

bool saveTagger(char* path)
{
    return Engine::singleton().saveTagger(path);
}

bool loadTagger(char* path)
{
    return Engine::singleton().loadTagger(path);
}

const size_t MAX_NAME_LENGTH = 16;
const size_t MAX_BUFFER_SIZE = (2 * MAX_FEATURES_PER_WORD + 1) * MAX_NAME_LENGTH;

bool getCompoundPOSTag(TagId tag, TagId* result, size_t* len)
{
    if (!result || !len)
    {
        spdlog::error("Result is null");
        return false;
    }

    const auto cpt = Engine::singleton().getEncoder().getCompoundPOSTag(tag);

    if (!cpt)
    {
        spdlog::error("failed to get compound tag for tag {}", tag);
        return false;
    }

    *len = 1;
    result[0] = cpt->POS;

    for (size_t f = 0; f < size_t(MAX_FEATURES_PER_WORD) && cpt->features[f].featureNameId != 0; ++f)
    {
        result[2 * f + 1] = cpt->features[f].featureNameId;
        result[2 * f + 2] = cpt->features[f].featureValueId;
        *len += 2;
    }

    return true;
}

bool describeTag(TagId tag, char** result, size_t* len)
{
    if (!result || !len)
    {
        spdlog::error("Result is null");
        return false;
    }

    const auto description = Engine::singleton().getEncoder().describePOSTag(tag);

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

bool saveTreeBuilder(char* path)
{
    return Engine::singleton().saveTreeBuilder(path);
}

bool loadTreeBuilder(char* path)
{
    return Engine::singleton().loadTreeBuilder(path);
}

bool buildDependencyTree(TagId* tags, size_t len, TagId* result)
{
    if (!result)
    {
        spdlog::error("Result is null");
        return false;
    }

    std::vector<TagId> t(len);
    std::copy(tags, tags + len, t.begin());

    std::optional<DepRelStatistics::Edges> res = Engine::singleton().buildDependencyTree(t);

    if (!res)
    {
        return false;
    }

    for (size_t i = 0; i < len; ++i)
    {
        result[3 * i] = (*res)[i].src;
        result[3 * i + 1] = (*res)[i].dest;
        result[3 * i + 2] = (*res)[i].label;
    }

    return true;
}

bool describeRel(TagId tag, char** result, size_t* len)
{
    if (!result || !len)
    {
        spdlog::error("Result is null");
        return false;
    }

    const auto description = Engine::singleton().getEncoder().describeDependencyRelationTag(tag);

    if (!description)
    {
        spdlog::error("failed to get description for tag {}", tag);
        return false;
    }

    *len = 2;

    static char buffer[MAX_BUFFER_SIZE];

    result[0] = buffer;
    char* pos = stpncpy(result[0], description->depRel.c_str(), MAX_NAME_LENGTH);
    result[1] = pos + 1;
    pos = strncpy(result[1], description->modifier.c_str(), MAX_NAME_LENGTH);

    return true;
}
