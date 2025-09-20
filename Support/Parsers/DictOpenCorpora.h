#pragma once

#include "Parser.h"
#include "../Collections/WordsCollection.h"
#include "../Collections/TagsCollection.h"
#include "../Collections/DepRelsCollection.h"
#include "../Engine/Sentence.h"

class DOCParser: public Parser
{
public:
    virtual ~DOCParser() {};

    virtual bool parse(const std::string& fileName, WordsCollection& wc, TagsCollection& tc, Sentences& sentences, Encoder& encoder, Printer& printer);
};
