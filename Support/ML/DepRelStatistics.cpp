#include "DepRelStatistics.h"

#include <fstream>

#include "spdlog/spdlog.h"

#include "../Math/MSTD.h"

void DepRelStatistics::processSentence(TagId root, const Sentence& sentence)
{
    for (const auto& word: sentence.words)
    {
        if (word.depHead > sentence.words.size())
        {
            spdlog::error("Error dependency head {} of maximum {}", word.depHead, sentence.words.size());
            continue;
        }
        ++stat.at(word.depRel, word.depHead == 0 ? root: sentence.words[word.depHead - 1].tags, word.tags);
    }
}

void DepRelStatistics::normalize(float smoothingFactor)
{
    spdlog::debug("Normalizing tree builder statistics {}", smoothingFactor);
    stat.normalizeLog(smoothingFactor, 2);
}

std::optional<DepRelStatistics::Edges> DepRelStatistics::extractGraph(TagId root, const std::vector<TagId>& tags)
{
    spdlog::debug("Extracting tree from graph for {} tags, {} labels, root {}", tags.size(), stat.sizeAt(0), root);

    DepRelStatistics::G g(tags.size() + 1, stat.sizeAt(0));

    for (TagId depRel = 0; depRel < stat.sizeAt(0); ++depRel)
    {
        for (TagId i1 = 0; i1 < tags.size(); ++i1)
        {
            g.addEdge(0, i1 + 1, depRel, stat.at(depRel, 0, tags[i1]));

            for (TagId i2 = 0; i2 < tags.size(); ++i2)
            {
                g.addEdge(i1 + 1, i2 + 1, depRel, stat.at(depRel, tags[i1], tags[i2]));
            }
        }
    }

    ChuLiuEdmondsMST solver(g);

    return solver.getSpanningTree(0);
}

void DepRelStatistics::saveBinary(ZLibFile& zfile) const
{
    stat.saveBinary(zfile);
}

bool DepRelStatistics::loadBinary(ZLibFile& zfile)
{
    return stat.loadBinary(zfile);
}
