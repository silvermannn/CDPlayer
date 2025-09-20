#pragma once

#include "Parser.h"
#include "../Collections/TagsCollection.h"
#include "../Collections/WordsCollection.h"
#include "../Engine/Sentence.h"
#include "../Engine/Encoder.h"

class DOCParser: public Parser
{
public:
    virtual ~DOCParser() {};

    virtual bool parse(const std::string& fileName, WordsCollection& wc, TagsCollection& tc, Sentences& sentences, Encoder& encoder, Printer& printer);
};
