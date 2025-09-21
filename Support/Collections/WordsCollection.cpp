#include "WordsCollection.h"

WordsCollection::WordsCollection()
{
    reset();
}

bool WordsCollection::operator==(const WordsCollection& other) const
{
    return _words2ids == other._words2ids && _ids2words == other._ids2words;
}

void WordsCollection::reset()
{
    _words2ids.clear();
    _ids2words.clear();

    _serviceWord = addInitialWord("<>");
    _unknownWord = addInitialWord("<unknown>");
}

WordId WordsCollection::addInitialWord(const std::string& word)
{
    return _words2ids.lookupOrInsert(word);
}

WordId WordsCollection::addWordForm(WordId initialForm, TagId tagId, const std::string& word)
{
    if (!isValidIndex(initialForm) || !isValidIndex(tagId))
    {
        return invalidIndex<WordId>();
    }
    
    Word w{initialForm, tagId};

    WordId id = _words2ids.lookupOrInsert(word);

    _ids2words.emplace(std::make_pair(id, w));

    return id;
}

WordId WordsCollection::serviceWord() const
{
    return _serviceWord;
}

WordId WordsCollection::unknownWord() const
{
    return _unknownWord;
}

size_t WordsCollection::wordsSize() const
{
    return _words2ids.size();
}

WordId WordsCollection::word2index(const std::string& word) const
{
    return _words2ids.lookup(word);
}

std::optional<std::string> WordsCollection::index2word(WordId word)
{
    if (!isValidIndex(word) || word > _words2ids.size())
    {
        return {};
    }
    
    return std::make_optional(_words2ids.lookupIndex(word));
}

bool WordsCollection::saveBinary(ZLibFile& zfile) const
{
    return _words2ids.saveBinary(zfile) && false;
}

bool WordsCollection::loadBinary(ZLibFile& zfile)
{
    return _words2ids.loadBinary(zfile) && false;
}
