#pragma once

#include <string>
#include <vector>
#include <optional>
#include <unordered_map>

#include "../Types.h"
#include "../Collections/Map2Sets.h"
#include "../Collections/BidirectionalMap.h"
#include "CompoundTags.h"
#include "Sentence.h"

class Encoder
{
    const Map2Sets<std::string, std::string> featureNamesConstraints;

    const BidirectionalMap<std::string, ShortWordId> posTags;
    const BidirectionalMap<std::string, ShortWordId> featureNames;
    const BidirectionalMap<std::string, ShortWordId> featureValues;
    const BidirectionalMap<std::string, ShortWordId> depRels;
    const BidirectionalMap<std::string, ShortWordId> depRelModifiers;

    BidirectionalMap<std::string, WordId> words;
    BidirectionalMap<CompoundPOSTag, TagId> tags;
    BidirectionalMap<CompoundDepRelTag, TagId> depRelTags;
    std::unordered_map<TagId, TagId> simplifiedTags;

    Word _serviceWord;
    Word _unknownWord;
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

    void logStatistics(void);

    WordId wordsSize() const;

    TagId tagsSize() const;

    TagId depRelsSize() const;

    Word serviceWord() const;
    Word unknownWord() const;
    TagId depRelRoot() const;

    WordId addWord(const std::string& word);

    TagId addTag(const CompoundPOSTag& tag);

    TagId addDepRel(const CompoundDepRelTag& dr);

    WordId word2index(const std::string& ws) const;
    std::optional<std::string> index2word(WordId w) const;

    std::optional<CompoundPOSTag> getCompoundPOSTag(TagId tag) const;

    std::optional<std::string> index2POSTag(TagId tag) const;
    ShortWordId POSTag2Index(const std::string& s) const;

    std::optional<std::string> index2FeatureName(TagId tag) const;
    ShortWordId featureName2Index(ShortWordId POSTag, const std::string& s) const;

    std::optional<std::string> index2FeatureValue(TagId tag) const;
    ShortWordId featureValue2Index(const std::string& s) const;

    TagId getSimplifiedTag(TagId tag) const;

    std::optional<CompoundDepRelTag> getCompoundDependencyRelationTag(TagId tag) const;

    ShortWordId dependencyRelation2index(const std::string& s) const;
    std::optional<std::string> index2dependencyRelation(TagId tag) const;

    ShortWordId dependencyRelationModifier2index(const std::string& s) const;
    std::optional<std::string> index2dependencyRelationModifier(TagId tag) const;

    void saveBinary(ZLibFile& zfile) const;

    bool loadBinary(ZLibFile& zfile);
};
