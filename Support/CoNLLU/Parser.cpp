#include "Parser.h"

#include <sstream>
#include <fstream>

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

bool CoNLLUParser::parse(const std::string& fileName, Sentences& sentences, Encoder& encoder)
{
    spdlog::info("Loading file {}", fileName);
    std::ifstream stream(fileName, std::fstream::in);

    if (stream.is_open())
    {
        Sentence sentence;
        size_t lines = 0;
        for (std::string line; std::getline(stream, line, '\n');)
        {
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

                // skip words counter wordData[0]

                filterNumbers(wordData[1]);
                word.word = encoder.addWord(wordData[1]);

                filterNumbers(wordData[2]);
                word.initialWord = encoder.addWord(wordData[2]);

                bool needToFillFeatures = true;
                size_t reassignCounter = 0;
                while (needToFillFeatures)
                {
                    needToFillFeatures = false;
                    CompoundPOSTag tag;

                    tag.POS = encoder.POSTag2Index(fixTag(wordData[3]));

                    if (!encoder.isValidIndex(tag.POS))
                    {
                        spdlog::warn("Unknown POS tag: '{}' -> 'x'", wordData[3]);
                        wordData[3] = "x";
                        continue;
                    }

                    std::string featuresLine;
                    if (wordData.size() > 4 && wordData[4] != "_") featuresLine += wordData[4];
                    if (wordData.size() > 5 && wordData[5] != "_") featuresLine += '|' + wordData[5];

                    std::vector<std::string> features = split(featuresLine, "/|");
                    std::sort(features.begin(), features.end());
                    size_t addedFeatures = 0;
                    for (auto featurePair: features)
                    {
                        std::string name, value;
                        if (!parsePair(featurePair, "=", name, value))
                        {
                            spdlog::warn("Wrong feature pair without '=': '{}' for POS tag '{}' in '{}'", featurePair, wordData[3], featuresLine);
                            continue;
                        }

                        std::string newPOS;
                        if (name.empty() || value.empty() || !fixFeatureName(name, wordData[3], newPOS)|| !fixFeatureValue(value))
                        {
                            spdlog::info("Ignored feature pair '{}' for POS tag '{}'", featurePair, wordData[3]);
                            if (!newPOS.empty())
                            {
                                spdlog::warn("New POS tag assigned: {} -> {} for features '{}'", wordData[3], newPOS, featuresLine);
                                wordData[3] = newPOS;
                                ++reassignCounter;
                                needToFillFeatures = true;
                                break;
                            }
                            continue;
                        }

                        ShortWordId fname = encoder.featureName2Index(tag.POS, name);
                        ShortWordId fvalue = encoder.featureValue2Index(value);
                        if (!encoder.isValidIndex(fname) || !encoder.isValidIndex(fvalue))
                        {
                            spdlog::warn("Unknown feature pair '{}={}/{}' for POS tag '{}'", name, value, featurePair, wordData[3]);
                        }
                        else
                        {
                            tag.features[addedFeatures].featureNameId = fname;
                            tag.features[addedFeatures].featureValueId = fvalue;
                            ++addedFeatures;
                        }

                        if (addedFeatures >= sizeof(tag.features) / sizeof(tag.features[0]))
                        {
                            spdlog::info("Maximum features number reached for POS tag '{}'", wordData[3]);
                            break;
                        }
                    }

                    if (needToFillFeatures)
                    {
                        continue;
                    }

                    word.tags = encoder.addTag(tag);
                }

                try
                {
                    if (wordData.size() > 6) word.depHead = std::stoul(wordData[6]);
                }
                catch(std::invalid_argument&)
                {
                    word.depHead = 0;
                }

                CompoundDepRelTag dr;
                if (wordData.size() > 7 && wordData[7] != "_")
                {
                    std::string depRelMain, depRelMod;
                    if (!parsePair(wordData[7], ":", depRelMain, depRelMod))
                    {
                        depRelMain = wordData[7];
                        depRelMod = "";
                    }

                    dr.depRel = encoder.dependencyRelation2index(depRelMain);
                    if (!encoder.isValidIndex(dr.depRel))
                    {
                        spdlog::warn("Unknown dependency relation '{}' for POS tag '{}'", depRelMain, wordData[3]);
                    }

                    dr.modifier = encoder.dependencyRelationModifier2index(depRelMod);
                    if (!encoder.isValidIndex(dr.modifier))
                    {
                        spdlog::warn("Unknown dependency relation modifier '{}:{}' for POS tag '{}'", depRelMain, depRelMod, wordData[3]);
                    }

                    word.depRel = encoder.addDepRel(dr);
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
