#pragma once

#include <string>

#include "../Collections/TagsCollection.h"
#include "../Collections/WordsCollection.h"
#include "../Collections/DepRelsCollection.h"
#include "../Engine/Sentence.h"
#include "../Engine/Printer.h"

struct Parser
{
    virtual bool parse(const std::string& fileName, WordsCollection& wc, TagsCollection& tc, DepRelsCollection& drc, Sentences& sentences, Printer& printer) = 0;

    virtual ~Parser() = 0;
};
