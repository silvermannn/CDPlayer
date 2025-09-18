#pragma once

#include <vector>
#include <optional>

#include "../Types.h"
#include "../Engine/Sentence.h"
#include "../Engine/Encoder.h"
#include "../ZLibFile/ZLibFile.h"
#include "../Math/Tensor.h"
#include "../Math/Graph.h"

class DepRelStatistics
{
    typedef Tensor<float, TagId, 3> T;
    typedef Graph<float, TagId> G;

    T stat;

public:
    typedef G::Edge Edge;
    typedef G::Edges Edges;

    DepRelStatistics()
        : stat()
    {
    }

    DepRelStatistics(TagId depRelsNum, TagId tagsNum)
        : stat(0, {depRelsNum, tagsNum, tagsNum})
    {
    }

    void resize(TagId depRelsNum, TagId tagsNum)
    {
        stat.resize(0, {depRelsNum, tagsNum, tagsNum});
    }

    void processSentence(const Encoder& encoder, const Sentence& sentence);

    void normalize(float smoothingFactor);

    std::optional<Edges> extractGraph(const Encoder& encoder, const std::vector<TagId>& tags);

    void saveBinary(ZLibFile& zfile) const;

    bool loadBinary(ZLibFile& zfile);
};
