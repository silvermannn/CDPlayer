#pragma once

#include <string>
#include <vector>
#include <utility>

#include <cstring>
#include <x86intrin.h>

#include "../Types.h"

const size_t MAX_FEATURES_PER_WORD = 16;

struct CompoundPOSTag
{
    ShortWordId POS = 0;
    struct
    {
        ShortWordId featureNameId = 0;
        ShortWordId featureValueId = 0;
    } features[MAX_FEATURES_PER_WORD];

    bool operator==(const CompoundPOSTag& other) const
    {
        return POS == other.POS && std::memcmp(features, other.features, sizeof(features)) == 0;
    }
};

template <>
struct std::hash<CompoundPOSTag>
{
  std::size_t operator()(const CompoundPOSTag& k) const
  {
    uint64_t res = (uint64_t)k.POS;
    for (const auto f: k.features)
    {
        res ^= uint64_t(f.featureNameId);
        _rotl(res, 8);
        res ^= uint64_t(f.featureValueId);
        _rotl(res, 8);
    }
    return std::hash<uint64_t>{}(res);
  }
};

struct CompoundPOSTagDescription
{
    std::string POS;
    std::vector<std::pair<std::string, std::string>> features;
};

struct CompoundDepRelTag
{
    ShortWordId depRel = 0;
    ShortWordId modifier = 0;

    bool operator==(const CompoundDepRelTag& other) const
    {
        return depRel == other.depRel && modifier == other.modifier;
    }
};

template <>
struct std::hash<CompoundDepRelTag>
{
  std::size_t operator()(const CompoundDepRelTag& k) const
  {
    return std::hash<uint64_t>{}((uint64_t)k.depRel ^ (uint64_t)k.modifier << 32);
  }
};

struct CompoundDepRelTagDescription
{
    std::string depRel;
    std::string modifier;
};

