#include <gtest/gtest.h>

#include "../Support/Engine/Encoder.h"

TEST(EncoderTest, Create)
{
    Encoder encoder;

    EXPECT_EQ(encoder.wordsSize(), 2);
    EXPECT_EQ(encoder.tagsSize(), 2);
    EXPECT_EQ(encoder.depRelsSize(), 0);
}

TEST(EncoderTest, Words)
{
    const std::string word("test");

    Encoder encoder;

    EXPECT_EQ(encoder.wordsSize(), 2);
    EXPECT_EQ(encoder.tagsSize(), 2);
    EXPECT_EQ(encoder.depRelsSize(), 0);

    EXPECT_TRUE(Encoder::isValidIndex(encoder.word2index(word)));
    WordId id = encoder.addWord(word);
    EXPECT_EQ(encoder.wordsSize(), 3);
    EXPECT_EQ(encoder.word2index(word), id);
    EXPECT_TRUE(Encoder::isValidIndex(encoder.word2index(word)));

    auto res = encoder.index2word(id);
    EXPECT_TRUE(res);
    EXPECT_EQ(*res, word);
}

TEST(EncoderTest, Tags)
{
    const std::string unk("unknownName");
    const std::string noun("noun");
    const std::string case1("case");
    const std::string acc("acc");

    Encoder encoder;

    EXPECT_EQ(encoder.wordsSize(), 2);
    EXPECT_EQ(encoder.tagsSize(), 2);
    EXPECT_EQ(encoder.depRelsSize(), 0);

    EXPECT_FALSE(Encoder::isValidIndex(encoder.POSTag2Index(unk)));
    EXPECT_FALSE(Encoder::isValidIndex(encoder.featureName2Index(0, unk)));
    EXPECT_FALSE(Encoder::isValidIndex(encoder.featureValue2Index(unk)));

    CompoundPOSTag tag;
    tag.POS = encoder.POSTag2Index(noun);
    tag.features[0].featureNameId = encoder.featureName2Index(tag.POS, case1);
    tag.features[0].featureValueId = encoder.featureValue2Index(acc);

    EXPECT_TRUE(Encoder::isValidIndex(tag.POS));
    EXPECT_TRUE(Encoder::isValidIndex(tag.features[0].featureNameId));
    EXPECT_TRUE(Encoder::isValidIndex(tag.features[0].featureValueId));

    TagId id = encoder.addTag(tag);
    EXPECT_EQ(encoder.tagsSize(), 3);

    {
        auto res = encoder.index2POSTag(1000);
        EXPECT_FALSE(res);
    }

    {
        auto res = encoder.getCompoundPOSTag(id);
        EXPECT_TRUE(res);
        EXPECT_EQ(*res, tag);
    }

    {
        auto res = encoder.index2POSTag(tag.POS);
        EXPECT_TRUE(res);
        EXPECT_EQ(*res, noun);
    }

    {
        auto res = encoder.index2FeatureName(tag.features[0].featureNameId);
        EXPECT_TRUE(res);
        EXPECT_EQ(*res, case1);
    }

    {
        auto res = encoder.index2FeatureValue(tag.features[0].featureValueId);
        EXPECT_TRUE(res);
        EXPECT_EQ(*res, acc);
    }
}

TEST(EncoderTest, Deprels)
{
    const std::string unk("unknownName");
    const std::string nsubj("nsubj");
    const std::string nummod("nummod");

    Encoder encoder;

    EXPECT_EQ(encoder.wordsSize(), 2);
    EXPECT_EQ(encoder.tagsSize(), 2);
    EXPECT_EQ(encoder.depRelsSize(), 0);

    EXPECT_FALSE(Encoder::isValidIndex(encoder.dependencyRelation2index(unk)));
    EXPECT_FALSE(Encoder::isValidIndex(encoder.dependencyRelationModifier2index(unk)));

    CompoundDepRelTag tag;
    tag.depRel = encoder.dependencyRelation2index(nsubj);
    tag.modifier = encoder.dependencyRelationModifier2index(nummod);

    EXPECT_TRUE(Encoder::isValidIndex(tag.depRel));
    EXPECT_TRUE(Encoder::isValidIndex(tag.modifier));

    TagId id = encoder.addDepRel(tag);
    EXPECT_EQ(encoder.depRelsSize(), 1);

    {
        auto res = encoder.index2dependencyRelation(1000);
        EXPECT_FALSE(res);
    }

    {
        auto res = encoder.index2dependencyRelationModifier(1000);
        EXPECT_FALSE(res);
    }

    {
        auto res = encoder.getCompoundDependencyRelationTag(id);
        EXPECT_TRUE(res);
        EXPECT_EQ(*res, tag);
    }

    {
        auto res = encoder.index2dependencyRelation(tag.depRel);
        EXPECT_TRUE(res);
        EXPECT_EQ(*res, nsubj);
    }

    {
        auto res = encoder.index2dependencyRelationModifier(tag.modifier);
        EXPECT_TRUE(res);
        EXPECT_EQ(*res, nummod);
    }
}

TEST(EncoderTest, SaveLoadEmpty)
{
    constexpr const char* fileName = "./encoder.bin.gz";

    Encoder e1;

    {
        ZLibFile zfile(fileName, true);

        EXPECT_TRUE(zfile.isOpen());

        e1.saveBinary(zfile);
    }

    Encoder e2;

    {
        ZLibFile zfile(fileName, false);

        EXPECT_TRUE(zfile.isOpen());

        EXPECT_TRUE(e2.loadBinary(zfile));
    }

    EXPECT_EQ(e1, e2);

    std::remove(fileName);
}

TEST(EncoderTest, SaveLoad)
{
    constexpr const char* fileName = "./encoder.bin.gz";
    const std::string word("test");
    const std::string noun("noun");
    const std::string case1("case");
    const std::string acc("acc");
    const std::string nsubj("nsubj");
    const std::string nummod("nummod");

    Encoder e1;
    e1.addWord(word);

    CompoundPOSTag ptag;
    ptag.POS = e1.POSTag2Index(noun);
    ptag.features[0].featureNameId = e1.featureName2Index(ptag.POS, case1);
    ptag.features[0].featureValueId = e1.featureValue2Index(acc);
    e1.addTag(ptag);

    CompoundDepRelTag drtag;
    drtag.depRel = e1.dependencyRelation2index(nsubj);
    drtag.modifier = e1.dependencyRelationModifier2index(nummod);
    e1.addDepRel(drtag);

    {
        ZLibFile zfile(fileName, true);

        EXPECT_TRUE(zfile.isOpen());

        e1.saveBinary(zfile);
    }

    Encoder e2;

    {
        ZLibFile zfile(fileName, false);

        EXPECT_TRUE(zfile.isOpen());

        EXPECT_TRUE(e2.loadBinary(zfile));
    }

    EXPECT_EQ(e1, e1);
    EXPECT_EQ(e1, e2);

    std::remove(fileName);
}