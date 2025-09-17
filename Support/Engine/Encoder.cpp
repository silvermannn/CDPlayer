#include "Encoder.h"

#include "StdDefs.h"

#include "spdlog/spdlog.h"

Encoder::Encoder()
    : featureNamesConstraints(TAG_DESCRIPTIONS)
    , posTags(TAG_DESCRIPTIONS)
    , featureValues(FEATURE_VALUES)
    , depRels(DEP_RELS)
    , depRelModifiers(DEP_RELS_MODIFIERS)
{
    CompoundPOSTag t;
    t.POS = posTags.lookup("x");
    unknownWord.tags = tags.lookupOrInsert(t);
    unknownWord.word = words.lookupOrInsert("");
}

void Encoder::reset()
{
    words.clear();
    tags.clear();

    CompoundPOSTag t(words.lookupOrInsert(defServiceTag));
    serviceTag = tags.lookupOrInsert(t);

    spdlog::info("Encoder service word/tag: {}", serviceTag);
    spdlog::info("Encoder unknown word {}, tag {}", unknownWord.tags, unknownWord.word);
}

ShortWordId Encoder::featureNameIndex(ShortWordId POSTag, const std::string& s) const
{
    if (featureNamesConstraints.check(posTags.lookupIndex(POSTag), s))
    {
        return featureNames.lookup(s);
    }

    return featureNames.invalidIndex;
}

ShortWordId Encoder::featureValueIndex(const std::string& s) const
{
    return featureValues.lookup(s);
}

ShortWordId Encoder::posTagIndex(const std::string& s) const
{
    return posTags.lookup(s);
}

void Encoder::logStatistics(void)
{
    spdlog::info("Encoder statistics:");
    spdlog::info("Words: {}", words.size());
    spdlog::info("Tags: {}", tags.size());
    spdlog::info("Dependency relation tags: {}", depRelTags.size());
}

WordId Encoder::serviceTagId()
{
    return serviceTag;
}

WordId Encoder::wordsSize() const
{
    return words.size();
}

TagId Encoder::tagsSize() const
{
    return tags.size();
}

TagId Encoder::depRelsSize() const
{
    return depRelTags.size();
}

WordId Encoder::addWord(const std::string& word)
{
    return words.lookupOrInsert(word);
}

TagId Encoder::addTag(const CompoundPOSTag& tag)
{
    return tags.lookupOrInsert(tag);
}

TagId Encoder::addDepRel(const CompoundDepRelTag& dr)
{
    return depRelTags.lookupOrInsert(dr);
}

WordId Encoder::word2index(const std::string& ws) const
{
    WordId res = words.lookup(ws);
    if(!isValidIndex(res))
    {
        res = unknownWord.word;
    }

    return res;
}

std::optional<std::string> Encoder::index2word(WordId w) const
{
    if (w >= words.size())
    {
        spdlog::error("Wrong word id {}", w);
        return {};
    }

    return std::make_optional(words.lookupIndex(w));
}

std::optional<CompoundPOSTag> Encoder::getCompoundPOSTag(TagId tag) const
{
    if (tag >= tags.size())
    {
        spdlog::error("Wrong tag id {}", tag);
        return {};
    }

    return std::make_optional(tags.lookupIndex(tag));
}

std::optional<std::string> Encoder::index2POSTag(TagId tag) const
{
    if (tag >= posTags.size())
    {
        spdlog::error("Wrong POS tag id {}", tag);
        return {};
    }

    return std::make_optional(posTags.lookupIndex(tag));
}

std::optional<std::string> Encoder::index2FeatureName(TagId tag) const
{
    if (tag >= featureNames.size())
    {
        spdlog::error("Wrong feature name tag id {}", tag);
        return {};
    }

    return std::make_optional(featureNames.lookupIndex(tag));
}

std::optional<std::string> Encoder::index2FeatureValue(TagId tag) const
{
    if (tag >= featureValues.size())
    {
        spdlog::error("Wrong feature value tag id {}", tag);
        return {};
    }

    return std::make_optional(featureValues.lookupIndex(tag));
}


ShortWordId Encoder::dependencyRelation2index(const std::string& s) const
{
    return depRels.lookup(s);
}

std::optional<std::string> Encoder::index2dependencyRelation(TagId tag) const
{
    if (tag >= depRelTags.size())
    {
        spdlog::error("Wrong dependency relation tag id {}", tag);
        return {};
    }

    return std::make_optional(depRels.lookupIndex(tag));
}

ShortWordId Encoder::dependencyRelationModifier2index(const std::string& s) const
{
    return depRelModifiers.lookup(s);
}

std::optional<std::string> Encoder::index2dependencyRelationModifier(TagId tag) const
{
    if (tag >= depRelTags.size())
    {
        spdlog::error("Wrong dependency relation modifier tag id {}", tag);
        return {};
    }

    return std::make_optional(depRelModifiers.lookupIndex(tag));
}

std::optional<CompoundDepRelTag> Encoder::getCompoundDependencyRelationTag(TagId tag) const
{
    if (tag >= depRelTags.size())
    {
        spdlog::error("Wrong dependency relation compound tag id {}", tag);
        return {};
    }

    return std::make_optional(depRelTags.lookupIndex(tag));
}

void Encoder::saveBinary(ZLibFile& zfile) const
{
    words.saveBinary(zfile);
    tags.saveBinary(zfile);
}

bool Encoder::loadBinary(ZLibFile& zfile)
{
    return words.loadBinary(zfile) && tags.loadBinary(zfile);
}
