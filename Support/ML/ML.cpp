#include "ML.h"

#include "spdlog/spdlog.h"

ML::ML()
{
}

ML::~ML()
{
    delete hmm;
    delete drStat;
}

void ML::trainHMMOnSentence(const Sentence& sentence)
{
    hmm->addHiddenState2HiddenState(serviceWord.tags, sentence.words[0].tags);
    hmm->addHiddenState2Emission(sentence.words[0].tags, sentence.words[0].word);

    for (size_t wix = 1; wix < sentence.words.size(); ++wix)
    {
        hmm->addHiddenState2HiddenState(sentence.words[wix-1].tags, sentence.words[wix].tags);
        hmm->addHiddenState2Emission(sentence.words[wix].tags, sentence.words[wix].word);
    }

    hmm->addHiddenState2HiddenState(sentence.words[sentence.words.size() - 1].tags, serviceWord.tags);
    hmm->addHiddenState2Emission(serviceWord.tags, serviceWord.word);
}

void ML::trainTagger(double smoothingFactor, WordId _wordsSize, TagId _tagsSize, TagId serviceTag, const Sentences& sentences)
{
    wordsSize = _wordsSize;
    tagsSize = _tagsSize;

    serviceWord.word = serviceTag;
    serviceWord.tags = serviceTag;

    delete hmm;
    hmm = new HMM<float, TagId, WordId>(tagsSize, wordsSize);

    trainHMMOnSentence(unkWordOnly);

    for (const auto& sentence: sentences)
    {
        trainHMMOnSentence(sentence);
    }

    hmm->normalize(smoothingFactor);
}

void ML::trainTreeBuilder(double smoothingFactor, TagId _depRelsSize, TagId _tagsSize, const Sentences& sentences)
{
    depRelsSize = _depRelsSize;
    tagsSize = _tagsSize;

    delete drStat;
    drStat = new DepRelStatistics(depRelsSize, tagsSize);

    for (const auto& sentence: sentences)
    {
        drStat->processSentence(sentence);
    }

    drStat->normalize(smoothingFactor);
}

std::vector<TagId> ML::tag(const std::vector<WordId>& sentence) const
{
    if (!hmm)
        return std::vector<TagId>();

    return hmm->predict(serviceWord.word, sentence);
}

void ML::saveBinary(ZLibFile& zfile) const
{
    zfile.write(wordsSize);
    zfile.write(tagsSize);
    zfile.write(depRelsSize);
    zfile.write(serviceWord.tags);

    if (hmm)
    {
        zfile.write(uint8_t(1));
        hmm->saveBinary(zfile);
    }
    else
    {
        zfile.write(uint8_t(0));
    }

    if (drStat)
    {
        zfile.write(uint8_t(1));
        drStat->saveBinary(zfile);
    }
    else
    {
        zfile.write(uint8_t(0));
    }
}

bool ML::loadBinary(ZLibFile& zfile)
{
    delete hmm;
    hmm = nullptr;

    delete drStat;
    drStat = nullptr;

    uint8_t present;

    if (!zfile.read(wordsSize) || !zfile.read(tagsSize) || !zfile.read(depRelsSize) || !zfile.read(serviceWord.tags) || !zfile.read(present))
    {
        return false;
    }

    serviceWord.word = serviceWord.tags;

    if (present == 1)
    {
        hmm = new HMM<float, TagId, WordId>(tagsSize, wordsSize);
        if (!hmm->loadBinary(zfile))
        {
            delete hmm;
            hmm = nullptr;
            return false;
        }
    }

    if (!zfile.read(present))
    {
        return false;
    }

    if (present == 1)
    {
        drStat = new DepRelStatistics(depRelsSize, tagsSize);
        if (!drStat->loadBinary(zfile))
        {
            delete drStat;
            drStat = nullptr;
            return false;
        }
    }

    return true;
}
