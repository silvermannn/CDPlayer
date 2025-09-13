#pragma once

#include <string>

#include "../Engine/Parser.h"
#include "../Engine/Sentence.h"
#include "../Engine/Encoder.h"

class CoNLLUParser: public Parser
{
public:
    virtual ~CoNLLUParser() {};

    virtual bool parse(const std::string& fileName, Sentences& sentences, Encoder& encoder);
};
