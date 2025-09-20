#pragma once

#include <string>
#include <vector>
#include <optional>
#include <unordered_map>

#include "../Types.h"
#include "Map2Sets.h"
#include "BidirectionalMap.h"
#include "CompoundTags.h"
#include "../Engine/Sentence.h"

class Encoder
{
    const Map2Sets<std::string, std::string> featureNamesConstraints;

    const BidirectionalMap<std::string, ShortWordId> posTags;
    const BidirectionalMap<std::string, ShortWordId> featureNames;
    const BidirectionalMap<std::string, ShortWordId> featureValues;
    const BidirectionalMap<std::string, ShortWordId> depRels;
    const BidirectionalMap<std::string, ShortWordId> depRelModifiers;

    BidirectionalMap<CompoundPOSTag, TagId> tags;
    BidirectionalMap<CompoundDepRelTag, TagId> depRelTags;

    std::unordered_map<TagId, TagId> simplifiedTags;

    TagId _depRelRoot;

    CompoundPOSTag simplify(const CompoundPOSTag& tag) const;

public:
    Encoder();

    bool operator==(const Encoder& other) const;

    void reset();

    template<typename Index>
    static bool isValidIndex(Index ix)
    {
        return BidirectionalMap<std::string, Index>::isValidIndex(ix);
    }

    TagId depRelsSize() const;

    TagId depRelRoot() const;

    TagId addTag(const CompoundPOSTag& tag);

    TagId addDepRel(const CompoundDepRelTag& dr);

    TagId getSimplifiedTag(TagId tag) const;

    std::optional<CompoundDepRelTag> getCompoundDependencyRelationTag(TagId tag) const;

    ShortWordId dependencyRelation2index(const std::string& s) const;
    std::optional<std::string> index2dependencyRelation(TagId tag) const;

    ShortWordId dependencyRelationModifier2index(const std::string& s) const;
    std::optional<std::string> index2dependencyRelationModifier(TagId tag) const;

    void saveBinary(ZLibFile& zfile) const;

    bool loadBinary(ZLibFile& zfile);
};
