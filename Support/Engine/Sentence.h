#pragma once

#include <vector>

#include "../Types.h"
#include "../ZLibFile/ZLibFile.h"

struct Word
{
    WordId word = 0;
    WordId initialWord = 0;
    TagId tags = 0;
    size_t depHead = 0;
    TagId depRel = 0;
};

struct Sentence
{
    std::vector<Word> words;

    void saveBinary(ZLibFile& zfile) const;
    bool loadBinary(ZLibFile& zfile);
};

typedef std::vector<Sentence> Sentences;
