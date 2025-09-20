#pragma once

#include <unordered_map>

#include <x86intrin.h>

#include "../ZLibFile/ZLibFile.h"

typedef uint8_t SimpleTagId;
typedef uint16_t TagId;

struct POSTag
{
    SimpleTagId POS = 0;
    std::unordered_map<SimpleTagId, SimpleTagId> features;

    bool operator==(const POSTag& other) const
    {
        return POS == other.POS && features == other.features;
    }
};

template<>
void ZLibFile::write<POSTag>(const POSTag& s);

template<>
bool ZLibFile::read<POSTag>(POSTag& s);

template <>
struct std::hash<POSTag>
{
  std::size_t operator()(const POSTag& k) const
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
