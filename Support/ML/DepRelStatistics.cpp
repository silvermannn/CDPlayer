#include "DepRelStatistics.h"

#include <fstream>
#include <cmath>

#include "spdlog/spdlog.h"

#include "../Math/MSTD.h"

void DepRelStatistics::processSentence(const Encoder& encoder, const Sentence& sentence)
{
    size_t zeroCount = 0;
    for (const auto& word: sentence.words)
    {
        if (word.depHead == 0)
            ++zeroCount;
        if (zeroCount > 2)
            return;
    }

    for (size_t i = 0; i < sentence.words.size(); ++i)
    {
        const auto& word = sentence.words[i];
        if (!encoder.isValidIndex(word.depHead))
        {
            continue;
        }
        if (word.depHead > sentence.words.size())
        {
            continue;
        }
        TagId src = word.depHead == 0 ? encoder.depRelRoot(): encoder.getSimplifiedTag(sentence.words[word.depHead - 1].tags);
        TagId dest = encoder.getSimplifiedTag(word.tags);
        ++stat.at(word.depRel, src, dest);

        (statistics[dest][word.depRel])++;
    }
}

void DepRelStatistics::normalize(float smoothingFactor)
{
    spdlog::debug("Normalizing tree builder statistics {}", smoothingFactor);
    stat.normalizeLog(smoothingFactor, 2);
}

std::optional<DepRelStatistics::Edges> DepRelStatistics::extractGraph(const Encoder& encoder, const std::vector<TagId>& tags)
{
    spdlog::debug("Extracting tree from graph for {} tags, {} labels, root {}", tags.size(), stat.sizeAt(0), encoder.depRelRoot());

    DepRelStatistics::G g(tags.size() + 1, stat.sizeAt(0));

    for (TagId depRel = 0; depRel < stat.sizeAt(0); ++depRel)
    {
        auto drTag = encoder.getCompoundDependencyRelationTag(depRel);
        if (!drTag)
        {
            continue;
        }
        for (TagId i1 = 0; i1 < tags.size(); ++i1)
        {
            TagId src = encoder.getSimplifiedTag(tags[i1]);

            g.addEdge(0, i1 + 1, depRel, stat.at(depRel, 0, src) - std::log(tags.size() + i1));

            for (TagId i2 = 0; i2 < tags.size(); ++i2)
            {
                TagId dest = encoder.getSimplifiedTag(tags[i2]);

                if (i1 == i2)
                    continue;

                if (drTag->headBefore != (i1 < i2))
                {
                    continue;
                }

                float distancePenalty = std::log(std::fabs(float(i1) - float(i2)));

                g.addEdge(i1 + 1, i2 + 1, depRel, stat.at(depRel, src, dest) - distancePenalty);
            }
        }
    }

    {
        std::ofstream s("dr-src.dot");
        g.saveDot(s);
    }

    ChuLiuEdmondsMST solver(g);

    auto p = solver.getSpanningTree(0);

    {
        std::ofstream s("dr.dot");
        g.saveDot(s);
    }

    return p;
}

void DepRelStatistics::saveBinary(ZLibFile& zfile) const
{
    stat.saveBinary(zfile);
}

bool DepRelStatistics::loadBinary(ZLibFile& zfile)
{
    return stat.loadBinary(zfile);
}

void DepRelStatistics::printStatistics(const Encoder& encoder) const
{
    std::ofstream stream("dr.csv");
    stream << "tags\t" << statistics.size() << std::endl;

    stream << "TAG/RELNAME" ;
    for (size_t dr = 0; dr < encoder.depRelsSize(); ++dr)
    {
        auto drTag = encoder.getCompoundDependencyRelationTag(dr);
        if (!drTag)
        {
            continue;
        }

        auto drName = encoder.index2dependencyRelation(drTag->depRel);
        if (!drName)
        {
            continue;
        }

        stream << "\t" << dr << ":" << *drName;

        auto drMod = encoder.index2dependencyRelationModifier(drTag->depRel);

        if (drMod)
        {
            stream << ":" << *drMod;
        }

        stream << (drTag->headBefore?"->":"<-");

    }

    stream << std::endl;

    for (const auto& [t, m]: statistics)
    {
        auto ct = encoder.getCompoundPOSTag(t);
        if (ct)
        {
            auto tn = encoder.index2POSTag(ct->POS);
            stream << *tn ;
        }
        for (size_t dr = 0; dr < encoder.depRelsSize(); ++dr)
        {
            auto v = m.find(dr);
            if (v == m.end())
                stream << "\t 0";
            else
                stream << "\t" << v->second;
        }
        stream << std::endl;
    }
}
