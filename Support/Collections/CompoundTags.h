#pragma once

#include <string>
#include <vector>
#include <utility>
#include <unordered_map>

#include <cstring>
#include <x86intrin.h>

#include "../Types.h"
#include "../ZLibFile/ZLibFile.h"

const size_t MAX_FEATURES_PER_WORD = 16;

struct CompoundPOSTag
{
    ShortWordId POS = 0;
    std::unordered_map<ShortWordId, ShortWordId> features;

    bool operator==(const CompoundPOSTag& other) const
    {
        return POS == other.POS && features == other.features;
    }
};

template<>
void ZLibFile::write<CompoundPOSTag>(const CompoundPOSTag& s);

template<>
bool ZLibFile::read<CompoundPOSTag>(CompoundPOSTag& s);

template <>
struct std::hash<CompoundPOSTag>
{
  std::size_t operator()(const CompoundPOSTag& k) const
  {
    uint64_t res = (uint64_t)k.POS;
    for (const auto& [k, v]: k.features)
    {
        res ^= uint64_t(k);
        _rotl(res, 8);
        res ^= uint64_t(v);
        _rotl(res, 8);
    }
    return std::hash<uint64_t>{}(res);
  }
};

struct CompoundDepRelTag
{
    ShortWordId depRel = 0;
    ShortWordId modifier = 0;
    bool headBefore = false;

    bool operator==(const CompoundDepRelTag& other) const
    {
        return depRel == other.depRel && modifier == other.modifier && headBefore == other.headBefore;
    }
};

template <>
struct std::hash<CompoundDepRelTag>
{
  std::size_t operator()(const CompoundDepRelTag& k) const
  {
    return std::hash<uint64_t>{}((uint64_t)k.depRel ^ (uint64_t)k.modifier << 32) ^ (uint64_t)k.headBefore << 17;
  }
};
