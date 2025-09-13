#pragma once

#include <vector>
#include <optional>

#include "../Types.h"
#include "../Engine/Sentence.h"
#include "../ZLibFile/ZLibFile.h"
#include "../Math/Tensor.h"
#include "../Math/Graph.h"

class DepRelStatistics
{
    typedef Tensor<float, ShortWordId, 3> T;
    typedef Graph<float, ShortWordId> G;

    T stat;

public:
    DepRelStatistics(ShortWordId depRelsNum, ShortWordId tagsNum)
        : stat(0, {depRelsNum, tagsNum, tagsNum})
    {
    }

    void processSentence(const Sentence& sentence);

    void normalize(float smoothingFactor);

    std::optional<G::Edges> extractGraph(const std::vector<ShortWordId>& tags);

    void saveBinary(ZLibFile& zfile) const;

    bool loadBinary(ZLibFile& zfile);
};
