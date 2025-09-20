#include "CompoundTags.h"

template<>
void ZLibFile::write<CompoundPOSTag>(const CompoundPOSTag& t)
{
    write(t.POS);

    uint32_t l = t.features.size();
    write(l);

    for (const auto& [k, v]: t.features)
    {
        write(k);
        write(v);
    }
}

template<>
bool ZLibFile::read<CompoundPOSTag>(CompoundPOSTag& t)
{
    if (!read(t.POS))
        return false;

    uint32_t l = 0;
    if (!read(l))
        return false;

    for (size_t i = 0; i < l; ++i)
    {
        ShortWordId k, v;
        if (!read(k) || !read(v))
        {
            return false;
        }

        t.features[k] = v;
    }

    return true;
}
