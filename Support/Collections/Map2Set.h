#pragma once

#include <unordered_map>
#include <unordered_set>
#include <iostream>

#include "spdlog/spdlog.h"

template <class K, class V>
class Map2Set
{
    std::unordered_map<K, std::unordered_set<V>> map;

public:
    template<typename Initializer>
    Map2Set(const std::vector<Initializer> initializers)
    {
        std::for_each(initializers.begin(), initializers.end(), [this](auto item)
        {
            std::for_each(item.items.begin(), item.items.end(), [&](auto subitem) {
                map[item.name].insert(subitem);
            });
        });
    }

    bool operator==(const Map2Set<K, V>& other) const
    {
        return map == other.map;
    }

    bool check(const K& k, const V& v) const
    {
        auto it = map.find(k);
        if (it == map.end())
        {
            spdlog::info("Key not found: {}", k);
            return false;
        }

        return it->second.contains(v);
    }

    void print(void) const
    {
        for (auto [k, v]: map)
        {
            std::cout << k << ":" << std::endl;
            for (auto i: v)
            {
                std::cout << i << ", ";
            }
            std::cout << std::endl;
        }
    }
};
