#pragma once

#include <cstddef>
#include <vector>
#include <functional>
#include <numeric>
#include <cmath>

#include <iostream>

#include "../ZLibFile/ZLibFile.h"

// N-dimensional matirx
template<typename N, typename IndexT, size_t Arity>
class Tensor
{
    struct Normalizer
    {
        N sum;
        N smoothongfactor;

        Normalizer(N s, IndexT size, N sf)
            : sum(s + sf * size)
            , smoothongfactor(sf)
        {
        }

        N operator()(N arg) const
        {
            return (arg + smoothongfactor) / sum;
        }
    };

    struct NormalizerLog
    {
        N sum;
        N smoothongfactor;

        NormalizerLog(N s, IndexT size, N sf)
            : sum(log(s + sf * size))
            , smoothongfactor(sf)
        {
        }

        N operator()(N arg) const
        {
            return log(arg + smoothongfactor) - sum;
        }
    };

    std::vector<IndexT> sizes;

    N* data = nullptr;
    N* sums = nullptr;
    IndexT sumsSize = 0;

    template<typename T, typename...Rest>
    IndexT getOffset(IndexT sizeIdx, T ix, Rest... indexes) const
    {
        return ix + getOffset(sizeIdx + 1, indexes...) * sizes[sizeIdx];
    }

    template<typename T>
    IndexT getOffset(IndexT, T ix) const
    {
        return ix;
    }

    void allocate(N initialValue)
    {
        data = new N[size()];
        std::fill(data, data + size(), initialValue);
    }

    void free()
    {
        delete data;
        delete sums;
    }

    N getAt(IndexT ix, IndexT i) const
    {
        if (ix == 0)
        {
            return getAt(ix, i, i);
        }

        N s = 0;
        for (IndexT j = 0; j < sizes[0]; ++j)
        {
            s += getAt(ix, i, j);
        }

        return s;
    }

    template<typename...Ints>
    requires (sizeof...(Ints) < Arity)
    N getAt(IndexT ix, IndexT i, Ints... indexes) const
    {
        if (ix == sizeof...(indexes))
        {
            return getAt(ix, i, indexes..., i);
        }

        N s = 0;
        for (IndexT j = 0; j < sizes[sizeof...(indexes)]; ++j)
        {
            s += getAt(ix, i, indexes..., j);
        }

        return s;
    }

    template<typename...Ints>
    requires (sizeof...(Ints) == Arity)
    N getAt(IndexT, IndexT, Ints... indexes) const
    {
        return at(indexes...);
    }

    void calculateSums(IndexT ix)
    {
        delete sums;

        sumsSize = sizes[ix];
        sums = new N[sumsSize];
        std::fill(sums, sums + sumsSize, 0);
        for (IndexT i = 0; i < sumsSize; ++i)
        {
            sums[i] = getAt(ix, i);
        }
    }

    template<typename Norm>
    void normalizeAt(Norm norm, N s, IndexT ix, IndexT i)
    {
        if (ix == 0)
        {
            return normalizeAt(norm, s, ix, i, i);
        }

        for (IndexT j = 0; j < sizes[0]; ++j)
        {
            normalizeAt(norm, s, ix, i, j);
        }
    }

    template<typename Norm, typename...Ints>
    requires (sizeof...(Ints) < Arity)
    void normalizeAt(Norm norm, N s, IndexT ix, IndexT i, Ints... indexes)
    {
        if (ix == sizeof...(indexes))
        {
            return normalizeAt(norm, s, ix, i, indexes..., i);
        }

        for (IndexT j = 0; j < sizes[sizeof...(indexes)]; ++j)
        {
            normalizeAt(norm, s, ix, i, indexes..., j);
        }
    }

    template<typename Norm, typename...Ints>
    requires (sizeof...(Ints) == Arity)
    void normalizeAt(Norm norm, N, IndexT, IndexT, Ints... indexes)
    {
        N& v = at(indexes...);
        v = norm(v);
    }

    template<typename Norm>
    void normalizeImpl(N smoothingFactor, IndexT ix)
    {
        calculateSums(ix);
        for (IndexT i = 0; i < sumsSize; ++i)
        {
            Norm norm(sums[i], N(size() / sumsSize), smoothingFactor);
            normalizeAt(norm, sums[i], ix, i);
        }
    }


public:
    Tensor()
        : sizes(Arity, 0)
    {
    }

    Tensor(N _initialValue, const IndexT(&_sizes)[Arity])
        : sizes(std::begin(_sizes), std::end(_sizes))
    {
        allocate(_initialValue);
    }

    ~Tensor()
    {
        free();
    }

    bool operator==(const Tensor<N, IndexT, Arity>& other) const
    {
        return sizes == other.sizes && memcmp(data, other.data, size() * sizeof(N)) == 0;
    }

    IndexT sizeAt(IndexT dim) const
    {
        return sizes[dim];
    }

    IndexT size() const
    {
        return std::accumulate(sizes.begin(), sizes.end(), IndexT(1), std::multiplies<IndexT>());
    }

    template<typename... Ints>
    const N& at(Ints... indexes) const
    {
        static_assert(sizeof...(indexes) == Arity);
        IndexT offset = getOffset(0, indexes...);
        return data[offset];
    }

    template<typename... Ints>
    N& at(Ints... indexes)
    {
        static_assert(sizeof...(indexes) == Arity);
        IndexT offset = getOffset(0, indexes...);
        return data[offset];
    }

    void normalize(N smoothingFactor, IndexT ix)
    {
        return normalizeImpl<Normalizer>(smoothingFactor, ix);
    }

    void normalizeLog(N smoothingFactor, IndexT ix)
    {
        return normalizeImpl<NormalizerLog>(smoothingFactor, ix);
    }

    void saveBinary(ZLibFile& zfile) const
    {
        zfile.write(uint8_t(Arity));
        for (IndexT i = 0; i < sizes.size(); ++i)
        {
            zfile.write(sizes[i]);
        }

        zfile.writePtr(data, size());
    }

    bool loadBinary(ZLibFile& zfile)
    {
        uint8_t arity = 0;
        zfile.read(arity);

        if (arity != Arity)
        {
            return false;
        }

        free();

        sizes.resize(Arity);
        for (IndexT i = 0; i < sizes.size(); ++i)
        {
            if (!zfile.read(sizes[i]) || sizes[i] == 0)
            {
                return false;
            }
        }

        allocate(0);

        return zfile.readPtr(data, size());
    }

    void print() const
    {
        for (IndexT i = 0; i < size(); ++i)
        {
            std::cout << data[i] << ", ";
        }
        std::cout << std::endl;

        if (sums)
        {
            for (IndexT i = 0; i < sumsSize; ++i)
            {
                std::cout << N(sums[i]) << ", ";
            }
        }
        else
            std::cout << " no sums ";
        std::cout << std::endl << std::endl;
   }
};
