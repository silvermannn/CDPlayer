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
    auto logger = spdlog::basic_logger_mt("support", "support.log", true);
    spdlog::set_default_logger(logger);
    spdlog::set_level(spdlog::level::debug);
    spdlog::flush_on(spdlog::level::debug);

    verb = encoder.POSTag2Index("verb");
    subCat = encoder.featureName2Index(verb, "subcat");
    subCatValues[0] = encoder.featureValue2Index("indir");
    subCatValues[1] = encoder.featureValue2Index("intr");
    subCatValues[2] = encoder.featureValue2Index("tran");
    subCatValues[3] = encoder.featureValue2Index("ditr");

    spdlog::info("Created Engine");

    static CoNLLUParser conlluParser;

    registerParser("CoNLLU", conlluParser);
    reset();
}

void Engine::reset(void)
{
    spdlog::info("Resetting");
    clearSentences();

    encoder.reset();

    unkWordOnly.words.push_back(encoder.unknownWord());

    spdlog::debug("Encoder service word {}, tag {}", encoder.serviceWord().tags, encoder.serviceWord().word);
    spdlog::debug("Encoder unknown word {}, tag {}", encoder.unknownWord().tags, encoder.unknownWord().word);
    spdlog::debug("Encoder dependency relation root {}", encoder.depRelRoot());
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

    if (!zfile.read(wordsSize) || !zfile.read(tagsSize))
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
            parseFile(dir_entry.path(), parserName);
            continue;
        }
    }

    return true;
}

bool Engine::parseFile(const std::string& path, const std::string& parserName)
{
    if (!std::filesystem::exists(path))
    {
        spdlog::error("Failed to open {}", path);
        return false;
    }

    auto parser = parsers.find(parserName);
    if (parser == parsers.end())
    {
        spdlog::error("Parser {} is not registered", parserName);
        return false;
    }

    if (!parser->second.parse(path, sentences, encoder, printer))
    {
        spdlog::error("Parser {} failed to load {}", parserName, path);
        return false;
    }

    return true;
}

bool Engine::parse(const std::string& path, const std::string& parserName)
{
    printer.init(std::string("Parsing ") + path, 1);

    if (std::filesystem::is_directory(path))
    {
        parseDirectory(path, parserName);
    }
    else
    {
        parseFile(path, parserName);
    }

    extractAdditionalInfo();

    encoder.logStatistics();
    return true;
}

void Engine::extractAdditionalInfo(void)
{
    printer.init(std::string("Extracting additional info"), sentences.size());
    for (auto& sentence: sentences)
    {
        extractAdditionalInfo(sentence);
    }
}

void Engine::extractAdditionalInfo(Sentence& sentence)
{
    printer.incProgress();
    // extract verb transitivity
    for (size_t i = 0; i < sentence.words.size(); ++i)
    {
        auto tag = encoder.getCompoundPOSTag(sentence.words[i].tags);
        size_t degree = 0;
        if (tag)
        {
            if(tag->POS == verb)
            {
                for (const auto& word: sentence.words)
                {
                    if (word.depHead == i - 1)
                    {
                        auto drTag = encoder.getCompoundDependencyRelationTag(word.depRel);
                        if (!drTag)
                        {
                            continue;
                        }

                        auto drName = encoder.index2dependencyRelation(drTag->depRel);
                        if (!drName)
                        {
                            continue;
                        }

                        if (degree < 1 && *drName == "nsubj")
                        {
                            degree = 1;
                        }

                        if (degree < 2 && *drName == "obj")
                        {
                            degree = 2;
                        }

                        if (degree < 3 && *drName == "iobj")
                        {
                            degree = 3;
                            break;
                        }
                    }
                }

                CompoundPOSTag res;
                res = *tag;
                bool found = false;
                size_t f = 0;
                for (; f < MAX_FEATURES_PER_WORD && res.features[f].featureNameId != 0; ++f)
                {
                    if (res.features[f].featureNameId == subCat)
                    {
                        res.features[f].featureValueId = subCatValues[degree];
                        found = true;
                        break;
                    }
                }
                if (!found)
                {
                    res.features[f].featureNameId = subCat;
                    res.features[f].featureValueId = subCatValues[degree];
                }

                sentence.words[i].tags = encoder.addTag(res);
            }
        }
    }
}

void Engine::trainHMMOnSentence(const Sentence& sentence)
{
    if (sentence.words.empty())
    {
        spdlog::debug("Trying to train tagger on empty sentence");
        return;
    }

    hmm.addHiddenState2HiddenState(encoder.serviceWord().tags, sentence.words[0].tags);
    hmm.addHiddenState2Emission(sentence.words[0].tags, sentence.words[0].word);

    for (size_t wix = 1; wix < sentence.words.size(); ++wix)
    {
        hmm.addHiddenState2HiddenState(sentence.words[wix-1].tags, sentence.words[wix].tags);
        hmm.addHiddenState2Emission(sentence.words[wix].tags, sentence.words[wix].word);
    }

    hmm.addHiddenState2HiddenState(sentence.words[sentence.words.size() - 1].tags, encoder.serviceWord().tags);
    hmm.addHiddenState2Emission(encoder.serviceWord().tags, encoder.serviceWord().word);
}


bool Engine::trainTagger(float smoothingFactor)
{
    printer.init(std::string("Training tagger"), sentences.size() + 1);

    hmm.resize(encoder.tagsSize(), encoder.wordsSize());

    trainHMMOnSentence(unkWordOnly);

    for (const auto& sentence: sentences)
    {
        printer.incProgress();
        trainHMMOnSentence(sentence);
    }

    printer.print("Normalizing tagger");
    printer.incProgress();
    hmm.normalize(smoothingFactor);

    return true;
}

std::optional<Tags> Engine::tag(const Words& sentence) const
{
    spdlog::debug("Tagging");

    return hmm.predict(encoder.serviceWord().word, sentence);
}

bool Engine::trainTreeBuilder(double smoothingFactor)
{
    printer.init(std::string("Training tree builder"), sentences.size() + 1);

    drStat.resize(encoder.depRelsSize(), encoder.tagsSize());

    for (const auto& sentence: sentences)
    {
        printer.incProgress();
        drStat.processSentence(encoder, sentence);
    }

    printer.print("Normalizing tree builder");
    printer.incProgress();
    drStat.normalize(smoothingFactor);

    drStat.printStatistics(encoder);

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
    spdlog::info("Build dependency tree");
    return drStat.extractGraph(encoder, tags);
}

Encoder& Engine::getEncoder()
{
    return encoder;
}
