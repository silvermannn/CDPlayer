#pragma once

#include <string>

#include "../Collections/TagsCollection.h"
#include "../Collections/WordsCollection.h"
#include "../Engine/Sentence.h"
#include "../Engine/Encoder.h"
#include "../Engine/Printer.h"

struct Parser
{
    virtual bool parse(const std::string& fileName, WordsCollection& wc, TagsCollection& tc, Sentences& sentences, Encoder& encoder, Printer& printer) = 0;

    virtual ~Parser() = 0;
};
