#include "Engine.h"

#include <filesystem>

#include "Utility.h"
#include "../ZLibFile/ZLibFile.h"
#include "../CoNLLU/Parser.h"

#include "spdlog/spdlog.h"
#include "spdlog/sinks/basic_file_sink.h"

const char MAGIC[] = "SUPPORTDB";

Engine& Engine::singleton()
{
    static Engine engine;

    return engine;
}

Engine::Engine()
{
    auto basic_sink = std::make_shared<spdlog::sinks::basic_file_sink_mt>("support.log");
    auto logger = std::make_shared<spdlog::logger>("support", basic_sink);
    spdlog::set_default_logger(logger);
    spdlog::set_level(spdlog::level::debug);
    spdlog::flush_on(spdlog::level::debug);

    spdlog::info("Init");

    static CoNLLUParser conlluParser;

    registerParser("CoNLLU", conlluParser);
    reset();
}

void Engine::reset(void)
{
    spdlog::info("Resetting");
    clearSentences();
    encoder.reset();
}

std::vector<std::string> Engine::availableParsers() const
{
    std::vector<std::string> res;

    res.reserve(parsers.size());

    for(const auto& kv : parsers)
    {
        res.push_back(kv.first);
    }

    return res;
}

bool Engine::registerParser(const std::string& parserName, Parser& parser)
{
    spdlog::info("Register parser {}", parserName);

    auto it = parsers.find(parserName);
    if (it != parsers.end())
    {
        spdlog::error("Parser {} is already registered", parserName);
        return false;
    }

    parsers.insert({parserName, parser});
    return true;
}

bool Engine::unregisterParser(const std::string& parserName)
{
    spdlog::info("Unregister parser {}", parserName);

    auto it = parsers.find(parserName);
    if (it == parsers.end())
    {
        spdlog::error("Parser {} is not registered", parserName);
        return false;
    }

    parsers.erase(parserName);
    return true;
}

void Engine::clearSentences(void)
{
    spdlog::info("Clearing sentences");
    sentences.clear();
}

bool Engine::saveSentences(const std::string& fileName) const
{
    spdlog::debug("Saving sentences {}", fileName);

    ZLibFile zfile(fileName, true);

    if (!zfile.isOpen())
    {
        spdlog::error("Could not open: {}", fileName);
        return false;
    }

    zfile.writePtr(MAGIC, sizeof(MAGIC));

    uint32_t size = sentences.size();
    zfile.write(size);

    for (const auto& sentence: sentences)
    {
        sentence.saveBinary(zfile);
    }

    return true;
}

bool Engine::loadSentences(const std::string& fileName)
{
    spdlog::info("Loading sentences");

    ZLibFile zfile(fileName, false);

    if (!zfile.isOpen())
    {
        spdlog::error("Could not open: {}", fileName);
        return false;
    }

    char buffer[sizeof(MAGIC)] = {0};

    if (!zfile.readPtr(buffer, sizeof(MAGIC)) || memcmp(buffer, MAGIC, sizeof(MAGIC)) != 0)
    {
        spdlog::error("Wrong magic");
        return false;
    }

    uint32_t size = 0;
    if (!zfile.read(size))
    {
        spdlog::error("Failed to load sentences number");
        return false;
    }

    sentences.resize(size);

    for (size_t i = 0; i < size; ++i)
    {
        if (!sentences[i].loadBinary(zfile))
        {
            spdlog::error("Failed to load sentence {} of {}", i, size);
            return false;
        }
    }

    return true;
}

bool Engine::saveEncoder(const std::string& fileName) const
{
    spdlog::debug("Saving encoder {}", fileName);

    ZLibFile zfile(fileName, true);

    if (!zfile.isOpen())
    {
        spdlog::error("Could not open: {}", fileName);
        return false;
    }

    zfile.writePtr(MAGIC, sizeof(MAGIC));

    encoder.saveBinary(zfile);

    return true;
}

bool Engine::loadEncoder(const std::string& fileName)
{
    spdlog::info("Loading encoder");

    ZLibFile zfile(fileName, false);

    if (!zfile.isOpen())
    {
        spdlog::error("Could not open: {}", fileName);
        return false;
    }

    char buffer[sizeof(MAGIC)] = {0};

    if (!zfile.readPtr(buffer, sizeof(MAGIC)) || memcmp(buffer, MAGIC, sizeof(MAGIC)) != 0)
    {
        spdlog::error("Wrong magic");
        return false;
    }

    if (!encoder.loadBinary(zfile))
    {
        spdlog::error("Failed to load encoder");
        return false;
    }

    encoder.logStatistics();
    return true;
}

bool Engine::saveTagger(const std::string& fileName) const
{
    spdlog::debug("Saving tagger {}", fileName);

    ZLibFile zfile(fileName, true);

    if (!zfile.isOpen())
    {
        spdlog::error("Could not open: {}", fileName);
        return false;
    }

    zfile.writePtr(MAGIC, sizeof(MAGIC));

    zfile.write(encoder.wordsSize());
    zfile.write(encoder.tagsSize());
    zfile.write(serviceWord.tags);

    hmm.saveBinary(zfile);

    return true;
}

bool Engine::loadTagger(const std::string& fileName)
{
    spdlog::info("Loading tagger");

    ZLibFile zfile(fileName, false);

    if (!zfile.isOpen())
    {
        spdlog::error("Could not open: {}", fileName);
        return false;
    }

    char buffer[sizeof(MAGIC)] = {0};

    if (!zfile.readPtr(buffer, sizeof(MAGIC)) || memcmp(buffer, MAGIC, sizeof(MAGIC)) != 0)
    {
        spdlog::error("Wrong magic");
        return false;
    }

    WordId wordsSize = 0;
    TagId tagsSize = 0;

    if (!zfile.read(wordsSize) || !zfile.read(tagsSize) || !zfile.read(serviceWord.tags))
    {
        return false;
    }

    hmm.resize(tagsSize, wordsSize);

    return hmm.loadBinary(zfile);
}

bool Engine::parseDirectory(const std::string& path, const std::string& parserName)
{
    spdlog::debug("Parse directory {}", path);

    for (auto const& dir_entry : std::filesystem::recursive_directory_iterator{path})
    {
        if (std::filesystem::is_regular_file(dir_entry))
        {
            parse(dir_entry.path(), parserName);
            continue;
        }
    }

    return true;
}

bool Engine::parse(const std::string& path, const std::string& parserName)
{
    spdlog::debug("Parsing {}", path);

    if (!std::filesystem::exists(path))
    {
        spdlog::error("Failed to open {}", path);
        return false;
    }

    if (std::filesystem::is_directory(path))
    {
        return parseDirectory(path, parserName);
    }

    auto parser = parsers.find(parserName);
    if (parser == parsers.end())
    {
        spdlog::error("Parser {} is not registered", parserName);
        return false;
    }

    if (!parser->second.parse(path, sentences, encoder))
    {
        spdlog::error("Parser {} failed to load {}", parserName, path);
        return false;
    }

    encoder.logStatistics();
    return true;
}

void Engine::trainHMMOnSentence(const Sentence& sentence)
{
    hmm.addHiddenState2HiddenState(serviceWord.tags, sentence.words[0].tags);
    hmm.addHiddenState2Emission(sentence.words[0].tags, sentence.words[0].word);

    for (size_t wix = 1; wix < sentence.words.size(); ++wix)
    {
        hmm.addHiddenState2HiddenState(sentence.words[wix-1].tags, sentence.words[wix].tags);
        hmm.addHiddenState2Emission(sentence.words[wix].tags, sentence.words[wix].word);
    }

    hmm.addHiddenState2HiddenState(sentence.words[sentence.words.size() - 1].tags, serviceWord.tags);
    hmm.addHiddenState2Emission(serviceWord.tags, serviceWord.word);
}


bool Engine::trainTagger(float smoothingFactor)
{
    spdlog::debug("Training tagger");

    serviceWord.word = encoder.serviceTagId();
    serviceWord.tags = encoder.serviceTagId();

    hmm.resize(encoder.tagsSize(), encoder.wordsSize());

    trainHMMOnSentence(unkWordOnly);

    for (const auto& sentence: sentences)
    {
        trainHMMOnSentence(sentence);
    }

    hmm.normalize(smoothingFactor);

    return true;
}

Strings Engine::tokenize(const std::string& sentence)
{
    std::string s(sentence);
    toLower(s);
    return split(s, " \t");
}

std::optional<Tags> Engine::tag(const Words& sentence) const
{
    spdlog::debug("Tagging");

    return hmm.predict(serviceWord.word, sentence);
}

bool Engine::trainTreeBuilder(double smoothingFactor)
{
    spdlog::info("Training tree builder");

    drStat.resize(encoder.depRelsSize(), encoder.tagsSize());

    for (const auto& sentence: sentences)
    {
        drStat.processSentence(sentence);
    }

    drStat.normalize(smoothingFactor);
    
    return true;
}

bool Engine::saveTreeBuilder(const std::string& fileName) const
{
    spdlog::debug("Saving dependency tree builder {}", fileName);

    ZLibFile zfile(fileName, true);

    if (!zfile.isOpen())
    {
        spdlog::error("Could not open: {}", fileName);
        return false;
    }

    zfile.writePtr(MAGIC, sizeof(MAGIC));

    zfile.write(encoder.tagsSize());
    zfile.write(encoder.depRelsSize());

    drStat.saveBinary(zfile);

    return true;
}

bool Engine::loadTreeBuilder(const std::string& fileName)
{
    spdlog::info("Loading dependency tree builder");

    ZLibFile zfile(fileName, false);

    if (!zfile.isOpen())
    {
        spdlog::error("Could not open: {}", fileName);
        return false;
    }

    char buffer[sizeof(MAGIC)] = {0};

    if (!zfile.readPtr(buffer, sizeof(MAGIC)) || memcmp(buffer, MAGIC, sizeof(MAGIC)) != 0)
    {
        spdlog::error("Wrong magic");
        return false;
    }

    TagId tagsSize = 0;
    TagId depRelsSize = 0;

    if (!zfile.read(tagsSize) || !zfile.read(depRelsSize))
    {
        return false;
    }

    drStat.resize(encoder.depRelsSize(), encoder.tagsSize());

    return drStat.loadBinary(zfile);
}

std::optional<DepRelStatistics::Edges> Engine::buildDependencyTree(const std::vector<TagId>& tags)
{
    return drStat.extractGraph(tags);
}

Encoder& Engine::getEncoder()
{
    return encoder;
}
