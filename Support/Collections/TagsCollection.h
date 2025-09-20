#pragma once

#include <optional>

#include "../Engine/POSTag.h"
#include "../ZLibFile/ZLibFile.h"
#include "BidirectionalMap.h"

class TagsCollection
{
    const BidirectionalMap<std::string, SimpleTagId> posTags;
    const BidirectionalMap<std::string, SimpleTagId> featureNames;
    const BidirectionalMap<std::string, SimpleTagId> featureValues;

    BidirectionalMap<POSTag, TagId> tags;

public:
    TagsCollection();
    
    TagId addTag(const POSTag& tag);

    std::optional<std::string> index2POSTag(SimpleTagId tag) const;
    SimpleTagId POSTag2Index(const std::string& s) const;

    std::optional<std::string> index2FeatureName(SimpleTagId tag) const;
    SimpleTagId featureName2Index(const std::string& s) const;

    std::optional<std::string> index2FeatureValue(SimpleTagId tag) const;
    SimpleTagId featureValue2Index(const std::string& s) const;

    bool saveBinary(ZLibFile& zfile) const;
    bool loadBinary(ZLibFile& zfile);
};
