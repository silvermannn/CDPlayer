#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <optional>

#include "../Types.h"
#include "Parser.h"
#include "Sentence.h"
#include "Encoder.h"
#include "../ML/ML.h"

#include "../ZLibFile/ZLibFile.h"

typedef std::vector<std::string> Strings;
typedef std::vector<TagId> Tags;

class Engine
{
    std::unordered_map<std::string, Parser&> parsers;

    std::vector<Sentence> sentences;

    Encoder encoder;

    ML ml;

    bool parseDirectory(const std::string& path, const std::string& parserName);

public:
    Engine();

    void reset(void);

    std::vector<std::string> availableParsers() const;

    bool registerParser(const std::string& parserName, Parser& parser);

    bool unregisterParser(const std::string& parserName);

    void clearSentences();

    void train(double smoothingFactor);

    Strings tokenize(const std::string& sentence);

    std::optional<Tags> tag(const Strings& sentence) const;

    std::optional<CompoundPOSTagDescription> describePOSTag(TagId tag) const;

    bool save(const std::string& fileName) const;

    bool load(const std::string& fileName);

    bool parse(const std::string& path, const std::string& parserName);
};
