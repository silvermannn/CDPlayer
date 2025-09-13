#pragma once

#include <string>

#include "Sentence.h"
#include "Encoder.h"

struct Parser
{
    virtual bool parse(const std::string& fileName, Sentences& sentences, Encoder& encoder) = 0;

    virtual ~Parser() = 0;
};
