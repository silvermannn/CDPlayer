#pragma once

#include <string>
#include <vector>
#include <optional>
#include <unordered_map>

#include "../Engine/POSTag.h"
#include "../ZLibFile/ZLibFile.h"
#include "BidirectionalMap.h"

typedef uint32_t WordId;

class WordsCollection
{
    struct Word
    {
        WordId _initialWord;
        TagId _posTag;
    };

    BidirectionalMap<std::string, WordId> _words2ids;

    std::unordered_map<WordId, Word> _ids2words;

    WordId _serviceWord;
    WordId _unknownWord;

public:

    WordId addInitialWord(const std::string& word);
    WordId addWordForm(WordId initialForm, TagId tagId, const std::string& word);

    size_t wordsSize() const;

    WordId findWord(const std::string& word);

    bool saveBinary(ZLibFile& zfile) const;
    bool loadBinary(ZLibFile& zfile);
};
