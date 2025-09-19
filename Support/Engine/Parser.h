#pragma once

#include <string>

#include "Sentence.h"
#include "Encoder.h"
#include "Printer.h"

struct Parser
{
    virtual bool parse(const std::string& fileName, Sentences& sentences, Encoder& encoder, Printer& printer) = 0;

    virtual ~Parser() = 0;
};
