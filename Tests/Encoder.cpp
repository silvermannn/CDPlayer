#include <gtest/gtest.h>

#include "../Support/Engine/Encoder.h"

TEST(EncoderTest, Create)
{
    Encoder encoder;

    EXPECT_EQ(encoder.wordsSize(), 1);
    EXPECT_EQ(encoder.tagsSize(), 1);
}