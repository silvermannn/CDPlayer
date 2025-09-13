#include <gtest/gtest.h>

#include <fstream>

#include "../Support/Support.h"
#include "../Support/Engine/Parser.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wwrite-strings"

TEST(SupportCInterfaceTest, Create)
{
    Handle h = init();

    EXPECT_TRUE(h != nullptr);

    clear(h);
}

static const char TestCoNLLU[] = " \n\
# sent_id = n01118003 \n\
# text = Drop the mic. \n\
1	Drop	drop	VERB	VB	VerbForm=Inf	0	root	0:root	_\n \
2	the	the	DET	DT	Definite=Def|PronType=Art	3	det	3:det	_ \n\
3	mic	mic	NOUN	NN	Number=Sing	1	obj	1:obj	SpaceAfter=No \n\
4	.	.	PUNCT	.	_	1	punct	1:punct	_ \n\
";

TEST(SupportCInterfaceTest, ParseCoNLLU)
{
    constexpr char* fileName = "./test.conllu";

    {
        std::ofstream test(fileName);
        test << TestCoNLLU;
        test.close();
    }

    Handle h = init();

    EXPECT_TRUE(h != nullptr);

    EXPECT_TRUE(load(h, fileName, "CoNLLU"));

    clear(h);

    std::remove(fileName);
}

struct FakeParser: public Parser
{
    bool success = false;

    virtual bool parse(const std::string&, Sentences&, Encoder&)
    {
        return success;
    };

    FakeParser() {};
    virtual ~FakeParser() {};
};

TEST(SupportCInterfaceTest, RegisterAndUseParser)
{
    constexpr char* parserName = "Fake";
    constexpr char* fileName = "./test.fake";

    {
        std::ofstream test(fileName);
    }

    FakeParser fakeParser;

    Handle h = init();

    EXPECT_TRUE(h != nullptr);

    EXPECT_TRUE(registerParser(h, parserName, &fakeParser));

    EXPECT_FALSE(registerParser(h, parserName, &fakeParser));

    EXPECT_FALSE(load(h, fileName, parserName));

    fakeParser.success = true;

    EXPECT_TRUE(load(h, fileName, parserName));

    EXPECT_TRUE(unregisterParser(h, parserName));

    EXPECT_FALSE(unregisterParser(h, parserName));

    clear(h);

    std::remove(fileName);
}

TEST(SupportCInterfaceTest, LoadNonExisting)
{
    constexpr char* fileName = "./.nonexisting.file";

    Handle h = init();

    EXPECT_TRUE(h != nullptr);

    std::remove(fileName);

    EXPECT_FALSE(load(h, fileName, "CoNLLU"));

    EXPECT_FALSE(load(h, fileName, "Native"));

    clear(h);

    std::remove(fileName);
}

TEST(SupportCInterfaceTest, SaveLoad)
{
    constexpr char* fileName = "./test.conllu";
    constexpr char* nativeFileName = "./test.bin.gz";

    {
        std::ofstream test(fileName);
        test << TestCoNLLU;
        test.close();
    }

    Handle h = init();

    EXPECT_TRUE(h != nullptr);

    EXPECT_TRUE(load(h, fileName, "CoNLLU"));

    EXPECT_TRUE(save(h, nativeFileName));

    EXPECT_TRUE(load(h, nativeFileName, "Native"));

    clear(h);

    std::remove(fileName);
    std::remove(nativeFileName);
}

#pragma GCC diagnostic pop
