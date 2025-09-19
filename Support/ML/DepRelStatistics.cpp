#include "DepRelStatistics.h"

#include <fstream>
#include <cmath>

#include "spdlog/spdlog.h"

#include "../Math/MSTD.h"

void DepRelStatistics::processSentence(const Encoder& encoder, const Sentence& sentence)
{
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

            if (stat.at(depRel, 0, src) == -INFINITY)
                continue;

            g.addEdge(0, i1 + 1, depRel, stat.at(depRel, 0, src) - 1);

            for (TagId i2 = 0; i2 < tags.size(); ++i2)
            {
                TagId dest = encoder.getSimplifiedTag(tags[i2]);

                if (stat.at(depRel, src, dest) == -INFINITY)
                    continue;

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
