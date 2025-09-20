#include "DepRelsCollection.h"

#include <algorithm>
#include <iterator>

#include "../Engine/StdDefs.h"

#include "spdlog/spdlog.h"

Encoder::Encoder()
    : featureNamesConstraints(TAG_DESCRIPTIONS)
    , posTags(TAG_DESCRIPTIONS, true)
    , featureNames(TAG_DESCRIPTIONS, false)
    //, featureValues(FEATURE_VALUES)
    //, depRels(DEP_RELS)
    //, depRelModifiers(DEP_RELS_MODIFIERS)
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
           tags == other.tags &&
           depRelTags == other.depRelTags;
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
    tags.clear();

    CompoundDepRelTag root;
    root.depRel = dependencyRelation2index("root");
    root.modifier = dependencyRelationModifier2index("");
    _depRelRoot = addDepRel(root);
}

TagId Encoder::depRelRoot() const
{
    return _depRelRoot;
}

TagId Encoder::depRelsSize() const
{
    return depRelTags.size();
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
    tags.saveBinary(zfile);
    depRelTags.saveBinary(zfile);
}

bool Encoder::loadBinary(ZLibFile& zfile)
{
    return tags.loadBinary(zfile) &&
           depRelTags.loadBinary(zfile);
}
