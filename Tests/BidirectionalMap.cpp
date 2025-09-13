#include <gtest/gtest.h>

#include "../Support/Collections/BidirectionalMap.h"

TEST(BidirectionalMapTest, CreateEmpty)
{
    BidirectionalMap<char, size_t> map;

    EXPECT_EQ(map.size(), 0);

    EXPECT_FALSE(map.isValidIndex(map.lookup(1)));
}

TEST(BidirectionalMapTest, Lookup)
{
    constexpr char a = 'a';

    BidirectionalMap<char, size_t> map;

    size_t index = map.lookupOrInsert(a);

    EXPECT_TRUE(map.isValidIndex(index));
    EXPECT_TRUE(map.isValidIndex(map.lookup(a)));

    EXPECT_EQ(map.size(), 1);

    EXPECT_EQ(map.lookupIndex(index), a);
}

TEST(BidirectionalMapTest, SaveLoad)
{
    constexpr const char* fileName = "./bmap.bin.gz";

    for(size_t t = 0; t < 10; ++t)
    {
        BidirectionalMap<std::string, size_t> map1;

        for (size_t j = 0; j < size_t(rand()); ++j)
        {
            map1.lookupOrInsert(std::to_string(rand()));
        }

        {
            ZLibFile zfile(fileName, true);

            EXPECT_TRUE(zfile.isOpen());

            map1.saveBinary(zfile);
        }

        BidirectionalMap<std::string, size_t> map2;

        {
            ZLibFile zfile(fileName, false);

            EXPECT_TRUE(zfile.isOpen());

            EXPECT_TRUE(map2.loadBinary(zfile));
        }

        EXPECT_EQ(map1, map2);
    }

    std::remove(fileName);
}