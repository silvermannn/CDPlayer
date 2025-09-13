#pragma once

#include <string>
#include <vector>
#include <optional>

#include "../Types.h"
#include "../Collections/BidirectionalMap.h"
#include "CompoundTags.h"
#include "Sentence.h"

struct TagFeatures
{
    size_t index;
    const BidirectionalMap<std::string, ShortWordId> items;

    TagFeatures(const std::vector<std::string> _items): items(_items) {};
    TagFeatures(size_t _index): index(_index) {};

    bool operator!=(const TagFeatures& other) const { return index != other.index; };

    operator size_t() const { return index; };
};

class Encoder
{
    const BidirectionalMap<std::string, TagFeatures> posTags;
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

    ShortWordId featureValueIndex(const std::string& s) const;

    ShortWordId dependencyRelation(const std::string& s) const;

    ShortWordId dependencyRelationModifier(const std::string& s) const;

    const TagFeatures& posTag(const std::string& s) const;

    void logStatistics(void);

    WordId wordsSize() const;

    TagId tagsSize() const;

    TagId depRelsSize() const;

    WordId serviceTagId();

    WordId addWord(const std::string& word);

    TagId addTag(const CompoundPOSTag& tag);

    TagId addDepRel(const CompoundDepRelTag& dr);

    std::vector<WordId> encodeWords(const std::vector<std::string>& ws) const;

    std::optional<CompoundPOSTagDescription> describePOSTag(TagId tag) const;

    void saveBinary(ZLibFile& zfile) const;

    bool loadBinary(ZLibFile& zfile);
};
