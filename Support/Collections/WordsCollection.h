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
    WordsCollection();

    WordId addInitialWord(const std::string& word);
    WordId addWordForm(WordId initialForm, TagId tagId, const std::string& word);

    WordId serviceWord() const;
    WordId unknownWord() const;

    size_t wordsSize() const;
    
    WordId word2index(const std::string& word) const;
    std::optional<std::string> index2word(WordId word);

    bool saveBinary(ZLibFile& zfile) const;
    bool loadBinary(ZLibFile& zfile);
};
