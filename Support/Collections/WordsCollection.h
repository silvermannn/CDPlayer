#pragma once

#include <string>
#include <vector>
#include <optional>
#include <unordered_map>

#include "POSTag.h"
#include "BidirectionalMap.h"
#include "../ZLibFile/ZLibFile.h"

typedef uint32_t WordId;

class WordsCollection
{
    struct Word
    {
        WordId _initialWord;
        TagId _posTag;
        
        bool operator==(const Word& other) const
        {
            return _initialWord == other._initialWord && _posTag == other._posTag;
        }
    };

    BidirectionalMap<std::string, WordId> _words2ids;

    std::unordered_map<WordId, Word> _ids2words;

    WordId _serviceWord;
    WordId _unknownWord;

public:
    WordsCollection();

    bool operator==(const WordsCollection& other) const;

    void reset();

    size_t wordsSize() const;
    
    WordId addInitialWord(const std::string& word);
    WordId addWordForm(WordId initialForm, TagId tagId, const std::string& word);

    WordId serviceWord() const;
    WordId unknownWord() const;

    WordId word2index(const std::string& word) const;
    std::optional<std::string> index2word(WordId word);

    bool saveBinary(ZLibFile& zfile) const;
    bool loadBinary(ZLibFile& zfile);
};
