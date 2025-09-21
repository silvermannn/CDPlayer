#include "CoNLLU.h"

#include <sstream>
#include <fstream>
#include <filesystem>

#include "../Engine/Utility.h"

#include "spdlog/spdlog.h"

void filterNumbers(std::string& s)
{
    bool replaced = false;
    std::replace_if(s.begin(), s.end(), [&replaced](unsigned char c){ bool r = std::isdigit(c); replaced = r || replaced; return r; }, 'N');
    if (replaced)
    {
        auto last = std::unique(s.begin(), s.end());
        s.erase(last, s.end());
    }
}

std::string fixTag(const std::string& s)
{
    if (s == "h") { return "adv"; }
    else if (s == "conj") { return "cconj"; }
    else return s;
}

bool fixFeatureName(std::string& s, const std::string& oldpos, std::string& pos)
{
    if (s.starts_with("form"))
    {
        s = "numform";
        return true;
    }
    if (s.starts_with("tran"))
    {
        s = "subcat";
        return true;
    }
    if (s.starts_with("anim"))
    {
        s = "animacy";
        return true;
    }
    if ((oldpos == "adj" || oldpos == "noun") && (s == "aspect" || s == "verbform" || s == "subcat" || s == "voice"))
    {
        pos = "verb";
        return false;
    }
    if (oldpos == "adj" && s == "nametype")
    {
        pos = "propn";
        return false;
    }
    if (s.starts_with("predic"))
    {
        // skip
        return false;
    }
    return true;
}

bool fixFeatureValue(std::string& s)
{
    if (s.starts_with("aor") || s.starts_with("notpast"))
    {
        s = "pres";
        return true;
    }
    if (s.starts_with("fut"))
    {
        s = "fut";
        return true;
    }
    if (s.starts_with("tran"))
    {
        s = "tran";
        return true;
    }
    if (s.starts_with("intr"))
    {
        s = "intr";
        return true;
    }
    if (s.starts_with("split") || s.starts_with("init"))
    {
        s = "yes";
        return true;
    }
    if (s.starts_with("ptr"))
    {
        s = "pat";
        return true;
    }
    if (s.starts_with("obsc"))
    {
        s = "vulg";
        return true;
    }
    if (s == "no" || s == "full" || s == "long")
    {
        return false;
    }
    return true;
}

bool CoNLLUParser::parse(const std::string& fileName, WordsCollection& wc, TagsCollection& tc, DepRelsCollection& drc, Sentences& sentences, Printer& printer)
{
    printer.init(std::string("Parse ") + fileName, std::filesystem::file_size(fileName));

    std::ifstream stream(fileName, std::fstream::in);

    if (stream.is_open())
    {
        Sentence sentence;
        size_t lines = 0;
        for (std::string line; std::getline(stream, line, '\n');)
        {
            printer.incProgress(line.size());
            ++lines;
            if (line.empty() || line.starts_with('#') || line.starts_with('='))
            {
                if (!sentence.words.empty())
                {
                    sentences.push_back(sentence);
                    sentence.words.clear();
                }
                continue;
            }

            toLower(line);

            if (line.starts_with('\t'))
            {
                line = '0' + line;
            }

            std::vector<std::string> wordData = split(line, "\t");

            if (wordData.size() > 3)
            {
                Word word;

                filterNumbers(wordData[2]);
                word.initialWord = wc.word2index(wordData[2]);
                if (!isValidIndex(word.initialWord))
                {
                    spdlog::warn("Unknown word initial form '{}' in '{}'", wordData[2], line);
                    word.initialWord = wc.unknownWord();
                }

                filterNumbers(wordData[1]);
                word.word = wc.word2index(wordData[1]);
                if (!isValidIndex(word.word))
                {
                    spdlog::warn("Unknown word '{}' in '{}'", wordData[1], line);
                    word.word = wc.unknownWord();
                }

                word.tags = wc.findTagForWord(word.word, word.initialWord);
                if (!isValidIndex(word.tags))
                {
                    spdlog::warn("Tag not found for word pair '{}' '{}' in '{}'", wordData[2], wordData[1], line);
                    word.tags = tc.unknownTag();
                }

                // Ignore tags for now.

                try
                {
                    if (wordData.size() > 6) word.depHead = std::stoul(wordData[6]);
                }
                catch(std::invalid_argument&)
                {
                    word.depHead = -1;
                }

                if (isValidIndex(word.depHead) && wordData.size() > 7 && wordData[7] != "_")
                {
                    DepRelTag dr;
                    dr.headBefore = word.depHead <= sentence.words.size();
                    std::string depRelMain, depRelMod;
                    if (!parsePair(wordData[7], ":", depRelMain, depRelMod))
                    {
                        depRelMain = wordData[7];
                        depRelMod = "";
                    }

                    dr.depRel = drc.dependencyRelation2index(depRelMain);
                    if (!isValidIndex(dr.depRel))
                    {
                        spdlog::warn("Unknown dependency relation '{}' for POS tag '{}' in '{}'", depRelMain, wordData[3], wordData[7]);
                    }

                    dr.modifier = drc.dependencyRelationModifier2index(depRelMod);
                    if (!isValidIndex(dr.modifier))
                    {
                        spdlog::warn("Unknown dependency relation modifier '{} : {}' for POS tag '{}' in '{}'", depRelMain, depRelMod, wordData[3], wordData[7]);
                    }

                    word.depRel = drc.addDepRel(dr);
                }
                else
                {
                    word.depHead = -1;
                }

                sentence.words.push_back(word);
            }
        }

        if (!sentence.words.empty())
        {
            sentences.push_back(sentence);
            sentence.words.clear();
        }
    }
    else
    {
        spdlog::error("Failed to open {}", fileName);
        return false;
    }

    return true;
}
