#include "Encoder.h"

#include "StdDefs.h"

#include "spdlog/spdlog.h"

Encoder::Encoder()
    : posTags(TAG_DESCRIPTIONS)
    , featureValues(FEATURE_VALUES)
    , depRels(DEP_RELS)
    , depRelModifiers(DEP_RELS_MODIFIERS)
{
    CompoundPOSTag t;
    t.POS = posTags.lookup("x").index;
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

ShortWordId Encoder::featureValueIndex(const std::string& s) const
{
    return featureValues.lookup(s);
}

ShortWordId Encoder::dependencyRelation(const std::string& s) const
{
    return depRels.lookup(s);
}

ShortWordId Encoder::dependencyRelationModifier(const std::string& s) const
{
    return depRelModifiers.lookup(s);
}


const TagFeatures& Encoder::posTag(const std::string& s) const
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

std::vector<WordId> Encoder::encodeWords(const std::vector<std::string>& ws) const
{
    std::vector<WordId> res(ws.size());

    for (size_t i = 0; i < ws.size(); ++i)
    {
        res[i] = words.lookup(ws[i]);
        if(!isValidIndex(res[i]))
        {
            res[i] = unknownWord.word;
        }
    }

    return res;
}

std::optional<CompoundPOSTagDescription> Encoder::describePOSTag(TagId tag) const
{
    if (tag >= tags.size())
    {
        spdlog::error("Wrong tag id {}", tag);
        return {};
    }

    const CompoundPOSTag& posTag = tags.lookupIndex(tag);

    CompoundPOSTagDescription description;

    description.POS = posTags.lookupIndex(posTag.POS);

    const TagFeatures& features = posTags.lookup(description.POS);

    description.features.reserve(MAX_FEATURES_PER_WORD);

    for(size_t i = 0; i < MAX_FEATURES_PER_WORD; ++i)
    {
        if (posTag.features[i].featureNameId == 0 || posTag.features[i].featureValueId == 0)
        {
            break;
        }
        auto p = std::make_pair<>(
            features.items.lookupIndex(posTag.features[i].featureNameId),
            featureValues.lookupIndex(posTag.features[i].featureValueId)
        );
        description.features.push_back(p);
    }

    return std::make_optional(description);
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
