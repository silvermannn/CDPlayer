#include "Engine.h"

#include <filesystem>

#include "Utility.h"

#include "spdlog/spdlog.h"
#include "spdlog/sinks/basic_file_sink.h"

const char MAGIC[] = "SUPPORTDB";

Engine::Engine()
{
    auto basic_sink = std::make_shared<spdlog::sinks::basic_file_sink_mt>("support.log");
    auto logger = std::make_shared<spdlog::logger>("support", basic_sink);
    spdlog::set_default_logger(logger);
    spdlog::set_level(spdlog::level::debug);
    spdlog::flush_on(spdlog::level::debug);

    spdlog::info("Init");

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

bool Engine::load(const std::string& fileName)
{
    spdlog::info("Loading binary");

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

    reset();

    if (!encoder.loadBinary(zfile))
    {
        spdlog::error("Failed to load encoder");
        return false;
    }

    if (!ml.loadBinary(zfile))
    {
        spdlog::error("Failed to load ml");
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

    encoder.logStatistics();
    return true;
}

bool Engine::parseDirectory(const std::string& path, const std::string& parserName)
{
    spdlog::debug("Loading directory {}", path);

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

bool Engine::save(const std::string& fileName) const
{
    spdlog::debug("Saving {}", fileName);

    ZLibFile zfile(fileName, true);

    if (!zfile.isOpen())
    {
        spdlog::error("Could not open: {}", fileName);
        return false;
    }

    zfile.writePtr(MAGIC, sizeof(MAGIC));

    encoder.saveBinary(zfile);

    ml.saveBinary(zfile);

    uint32_t size = sentences.size();
    zfile.write(size);

    for (const auto& sentence: sentences)
    {
        sentence.saveBinary(zfile);
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

void Engine::train(double smoothingFactor)
{
    spdlog::debug("Training");

    ml.trainTagger(smoothingFactor , encoder.wordsSize(), encoder.tagsSize(), encoder.serviceTagId(), sentences);
    //ml.trainTreeBuilder(smoothingFactor, encoder.depRels.size(), encoder.tagsSize(), sentences);
}

Strings Engine::tokenize(const std::string& sentence)
{
    std::string s(sentence);
    toLower(s);
    return split(s, " \t");
}

std::optional<Tags> Engine::tag(const Strings& sentence) const
{
    spdlog::debug("Tagging");
    std::vector<WordId> encoded = encoder.encodeWords(sentence);

    return ml.tag(encoded);
}

std::optional<CompoundPOSTagDescription> Engine::describePOSTag(TagId tag) const
{
    return encoder.describePOSTag(tag);
}

