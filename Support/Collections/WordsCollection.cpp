#include "WordsCollection.h"

WordId WordsCollection::addInitialWord(const std::string& word)
{
    return _words2ids.lookupOrInsert(word);
}

WordId WordsCollection::addWordForm(WordId initialForm, TagId tagId, const std::string& word)
{
    Word w{initialForm, tagId};

    WordId id = _words2ids.lookupOrInsert(word);

    _ids2words.emplace(std::make_pair(id, w));

    return id;
}

size_t WordsCollection::wordsSize() const
{
    return _words2ids.size();
}

WordId WordsCollection::findWord(const std::string& word)
{
    return _words2ids.lookup(word);
}

bool WordsCollection::saveBinary(ZLibFile& zfile) const
{
    return false;
}

bool WordsCollection::loadBinary(ZLibFile& zfile)
{
    return false;
}
