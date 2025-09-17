#pragma once

#include <string>
#include <vector>
#include <optional>

#include "../Types.h"
#include "../Collections/Map2Set.h"
#include "../Collections/BidirectionalMap.h"
#include "CompoundTags.h"
#include "Sentence.h"

class Encoder
{
    const Map2Set<std::string, std::string> featureNamesConstraints;

    const BidirectionalMap<std::string, ShortWordId> posTags;
    const BidirectionalMap<std::string, ShortWordId> featureNames;
    const BidirectionalMap<std::string, ShortWordId> featureValues;
    const BidirectionalMap<std::string, ShortWordId> depRels;
    const BidirectionalMap<std::string, ShortWordId> depRelModifiers;

    BidirectionalMap<std::string, WordId> words;
    BidirectionalMap<CompoundPOSTag, TagId> tags;
    BidirectionalMap<CompoundDepRelTag, TagId> depRelTags;

    WordId serviceTag = 0;
    Word unknownWord;

public:
    Encoder();

    void reset();

    template<typename Index>
    bool isValidIndex(Index ix) const
    {
        return BidirectionalMap<std::string, Index>::isValidIndex(ix);
    }

    ShortWordId featureNameIndex(ShortWordId POSTag, const std::string& s) const;

    ShortWordId featureValueIndex(const std::string& s) const;

    ShortWordId posTagIndex(const std::string& s) const;

    void logStatistics(void);

    WordId wordsSize() const;

    TagId tagsSize() const;

    TagId depRelsSize() const;

    WordId serviceTagId();

    WordId addWord(const std::string& word);

    TagId addTag(const CompoundPOSTag& tag);

    TagId addDepRel(const CompoundDepRelTag& dr);

    WordId word2index(const std::string& ws) const;
    std::optional<std::string> index2word(WordId w) const;

    std::optional<CompoundPOSTag> getCompoundPOSTag(TagId tag) const;
    std::optional<std::string> index2POSTag(TagId tag) const;
    std::optional<std::string> index2FeatureName(TagId tag) const;
    std::optional<std::string> index2FeatureValue(TagId tag) const;

    std::optional<CompoundDepRelTag> getCompoundDependencyRelationTag(TagId tag) const;

    ShortWordId dependencyRelation2index(const std::string& s) const;
    std::optional<std::string> index2dependencyRelation(TagId tag) const;

    ShortWordId dependencyRelationModifier2index(const std::string& s) const;
    std::optional<std::string> index2dependencyRelationModifier(TagId tag) const;


    void saveBinary(ZLibFile& zfile) const;

    bool loadBinary(ZLibFile& zfile);
};
