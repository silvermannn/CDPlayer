#include "ZLibFile.h"

#include <spdlog/spdlog.h>

constexpr size_t MAX_STRING_LEN = 1024;

ZLibFile::ZLibFile(const std::string& filename, bool write)
{
    fileHande = gzopen(filename.c_str(), write?"wb":"rb");

    fileIsOpen = fileHande != Z_NULL;

    if (!isOpen())
    {
        spdlog::error("Failed to open file {}: {}", filename, zError(errno));
    }
}

ZLibFile::~ZLibFile()
{
    gzclose(fileHande);
};

template<>
void ZLibFile::write<std::string>(const std::string& s)
{
    uint32_t l = std::min(s.length(), MAX_STRING_LEN - 1);
    write(l);
    writePtr(s.c_str(), l);
}

template<>
bool ZLibFile::read<std::string>(std::string& s)
{
    uint32_t l = 0;
    if (!read(l))
        return false;

    l = std::min(size_t(l), MAX_STRING_LEN - 1);

    static char buffer[MAX_STRING_LEN];

    if (!readPtr((char*)buffer, l))
        return false;

    buffer[l] = '\0';
    s = buffer;

    return true;
}
