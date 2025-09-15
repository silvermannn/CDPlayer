#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <optional>

#include "../Types.h"
#include "../ML/HMM.h"
#include "../ML/DepRelStatistics.h"
#include "Parser.h"
#include "Sentence.h"
#include "Encoder.h"

typedef std::vector<std::string> Strings;
typedef std::vector<TagId> Tags;

class Engine
{
    std::unordered_map<std::string, Parser&> parsers;

    std::vector<Sentence> sentences;

    Encoder encoder;

    HMM<float, TagId, WordId> hmm;

    DepRelStatistics drStat;

    Sentence unkWordOnly;
    Word unknownWord;
    Word serviceWord;

    bool parseDirectory(const std::string& path, const std::string& parserName);

    void trainHMMOnSentence(const Sentence& sentence);
public:
    Engine();

    void reset(void);

    std::vector<std::string> availableParsers() const;

    bool registerParser(const std::string& parserName, Parser& parser);

    bool unregisterParser(const std::string& parserName);

    void clearSentences();

    bool trainTagger(float smoothingFactor);

    void trainTreeBuilder(double smoothingFactor);

    Strings tokenize(const std::string& sentence);

    bool parse(const std::string& path, const std::string& parserName);

    bool saveSentences(const std::string& fileName) const;

    bool loadSentences(const std::string& fileName);

    bool saveEncoder(const std::string& fileName) const;

    bool loadEncoder(const std::string& fileName);

    std::optional<Tags> tag(const Strings& sentence) const;

    std::optional<CompoundPOSTag> getCompoundPOSTag(TagId tag) const;

    std::optional<CompoundPOSTagDescription> describePOSTag(TagId tag) const;

    bool saveTagger(const std::string& fileName) const;

    bool loadTagger(const std::string& fileName);

    bool saveTreeBuilder(const std::string& fileName) const;

    bool loadTreeBuilder(const std::string& fileName);

    std::optional<DepRelStatistics::Edges> buildDependencyTree(const std::vector<TagId>& tags);

    std::optional<CompoundDepRelTagDescription> describeDependencyRelationTag(TagId tag) const;
};
