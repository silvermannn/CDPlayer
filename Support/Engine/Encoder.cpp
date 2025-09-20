#include "Encoder.h"

#include <algorithm>
#include <iterator>

#include "StdDefs.h"

#include "spdlog/spdlog.h"

Encoder::Encoder()
    : featureNamesConstraints(TAG_DESCRIPTIONS)
    , posTags(TAG_DESCRIPTIONS, true)
    , featureNames(TAG_DESCRIPTIONS, false)
    , featureValues(FEATURE_VALUES)
    , depRels(DEP_RELS)
    , depRelModifiers(DEP_RELS_MODIFIERS)
{
    reset();
}

bool Encoder::operator==(const Encoder& other) const
{
    return featureNamesConstraints == other.featureNamesConstraints &&
           posTags == other.posTags &&
           featureNames == other.featureNames &&
           featureValues == other.featureValues &&
           depRels == other.depRels &&
           depRelModifiers == other.depRelModifiers &&
           words == other.words &&
           tags == other.tags &&
           depRelTags == other.depRelTags &&
           _serviceWord == other._serviceWord &&
           _unknownWord == other._unknownWord;
}

CompoundPOSTag Encoder::simplify(const CompoundPOSTag& tag) const
{
    CompoundPOSTag res;
    res.POS = tag.POS;
    auto pos = posTags.lookupIndex(res.POS);
    std::copy_if(tag.features.begin(), tag.features.end(), std::inserter(res.features, res.features.end()),
                 [&](const auto& k) { auto fn = featureNames.lookupIndex(k.first); return featureNamesConstraints.checkIsSimple(pos, fn); });
    return res;
}

void Encoder::reset()
{
    words.clear();
    tags.clear();

    CompoundPOSTag dt;
    dt.POS = 0;
    _serviceWord.tags = tags.lookupOrInsert(dt);
    _serviceWord.word = words.lookupOrInsert(defServiceTag);

    CompoundPOSTag ut;
    ut.POS = posTags.lookup("x");
    _unknownWord.tags = tags.lookupOrInsert(ut);
    _unknownWord.word = words.lookupOrInsert("<unknown>");

    CompoundDepRelTag root;
    root.depRel = dependencyRelation2index("root");
    root.modifier = dependencyRelationModifier2index("");
    _depRelRoot = addDepRel(root);
}

Word Encoder::serviceWord() const
{
    return _serviceWord;
}

Word Encoder::unknownWord() const
{
    return _unknownWord;
}

TagId Encoder::depRelRoot() const
{
    return _depRelRoot;
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

void Encoder::addWordInitialForm(WordId word, WordId initialWord)
{
    initialForms[word] = initialWord;
}

TagId Encoder::addTag(const CompoundPOSTag& tag)
{
    auto res = tags.lookupOrInsert(tag);
    simplifiedTags[res] = tags.lookupOrInsert(simplify(tag));
    return res;
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
        res = _unknownWord.word;
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

WordId Encoder::getInitialWord(WordId word)
{
    return initialForms[word];
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

ShortWordId Encoder::featureName2Index(ShortWordId POSTag, const std::string& s) const
{
    auto pos = posTags.lookupIndex(POSTag);
    if (featureNamesConstraints.check(pos, s))
    {
        return featureNames.lookup(s);
    }

    return featureNames.invalidIndex;
}

ShortWordId Encoder::featureValue2Index(const std::string& s) const
{
    return featureValues.lookup(s);
}

ShortWordId Encoder::POSTag2Index(const std::string& s) const
{
    return posTags.lookup(s);
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

TagId Encoder::getSimplifiedTag(TagId tag) const
{
    const auto& res = simplifiedTags.find(tag);
    if (res == simplifiedTags.end())
    {
        return tags.invalidIndex;
    }

    return res->second;
}

ShortWordId Encoder::dependencyRelation2index(const std::string& s) const
{
    return depRels.lookup(s);
}

std::optional<std::string> Encoder::index2dependencyRelation(TagId tag) const
{
    if (tag >= depRels.size())
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
    if (tag >= depRelModifiers.size())
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
    depRelTags.saveBinary(zfile);
}

bool Encoder::loadBinary(ZLibFile& zfile)
{
    return words.loadBinary(zfile) &&
           tags.loadBinary(zfile) &&
           depRelTags.loadBinary(zfile);
}
