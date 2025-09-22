#include "WordsCollection.h"

WordsCollection::WordsCollection()
{
    reset();
}

bool WordsCollection::operator==(const WordsCollection& other) const
{
    return _words2ids == other._words2ids && _wids2tags == other._wids2tags;
}

void WordsCollection::reset()
{
    _words2ids.clear();
    _wids2tags.clear();

    _serviceWord = addWord("<>");
    _unknownWord = addWord("<unknown>");
}

WordId WordsCollection::addWord(const std::string& word)
{
    return _words2ids.lookupOrInsert(word);
}

WordId WordsCollection::addWordForm(TagId tagId, const std::string& word)
{
    if (!isValidIndex(tagId))
    {
        return invalidIndex<WordId>();
    }

    WordId id = _words2ids.lookupOrInsert(word);

    _wids2tags.emplace(std::make_pair(id, tagId));

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

std::vector<TagId> WordsCollection::findTagsForWord(WordId word) const
{
    const auto& its = _wids2tags.equal_range(word);

    std::vector<TagId> res;
    std::transform(its.first, its.second, std::back_inserter(res), [](const auto& it){ return it.second; });
    return res;
}

bool WordsCollection::saveBinary(ZLibFile& zfile) const
{
    return _words2ids.saveBinary(zfile) && zfile.write(_wids2tags);
}

bool WordsCollection::loadBinary(ZLibFile& zfile)
{
    return _words2ids.loadBinary(zfile) && zfile.read(_wids2tags);
}
