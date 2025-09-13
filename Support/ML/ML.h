#pragma once

#include <string>
#include <vector>

#include "HMM.h"
#include "DepRelStatistics.h"

#include "../ZLibFile/ZLibFile.h"
#include "../Engine/Sentence.h"

class ML
{
    HMM<float, TagId, WordId>* hmm = nullptr;
    DepRelStatistics* drStat = nullptr;

    WordId wordsSize = 0;
    TagId tagsSize = 0;
    TagId depRelsSize = 0;

    Sentence unkWordOnly;
    Word unknownWord;
    Word serviceWord;

    void trainHMMOnSentence(const Sentence& sentence);
public:
    ML();
    ~ML();

    void trainTagger(double smoothingFactor, WordId _wordsSize, TagId _tagsSize, TagId serviceTag, const Sentences& sentences);

    void trainTreeBuilder(double smoothingFactor, TagId _depRelsSize, TagId _tagsSize, const Sentences& sentences);

    std::vector<TagId> tag(const std::vector<WordId>& sentence) const;

    void saveBinary(ZLibFile& zfile) const;

    bool loadBinary(ZLibFile& zfile);
};

