#include "DepRelStatistics.h"

#include "../Math/MSTD.h"

void DepRelStatistics::processSentence(const Sentence& sentence)
{
    for(const auto& word: sentence.words)
    {
        ++stat.at(word.depRel, word.depHead == 0 ? 0: sentence.words[word.depHead].tags, word.tags);
    }
}

void DepRelStatistics::normalize(float smoothingFactor)
{
    stat.normalize(smoothingFactor, 2);
}

std::optional<DepRelStatistics::G::Edges> DepRelStatistics::extractGraph(const std::vector<ShortWordId>& tags)
{
    DepRelStatistics::G g(tags.size(), stat.sizeAt(0));

    for (ShortWordId depRel = 0; depRel < stat.sizeAt(0); ++depRel)
    {
        for (ShortWordId i1 = 0; i1 < tags.size(); ++i1)
        {
            for (ShortWordId i2 = 0; i2 < tags.size(); ++i2)
            {
                g.addEdge(i1, i2, depRel, stat.at(depRel, tags[i1], tags[i2]));
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
