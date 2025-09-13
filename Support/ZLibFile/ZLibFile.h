#pragma once

#include <string>
#include <zlib.h>

class ZLibFile
{
    gzFile fileHande;
    bool fileIsOpen = false;

    const uint64_t CHUNK_SIZE = 1024 * 1024;

public:
    ZLibFile(const std::string& filename, bool write);
    ~ZLibFile();

    bool isOpen()
    {
        return fileIsOpen;
    }

    template<typename T>
    void write(const T& t)
    {
        gzwrite(fileHande, reinterpret_cast<const char*>(&t), sizeof(T));
    }

    template<typename T>
    void writePtr(const T* const t, uint64_t len)
    {
        const uint64_t chunks = (sizeof(T) * len) / CHUNK_SIZE;
        const uint64_t rest = (sizeof(T) * len) % CHUNK_SIZE;

        for (uint64_t i = 0; i < chunks; ++i)
        {
            gzwrite(fileHande, reinterpret_cast<const char*>(t) + i * CHUNK_SIZE, CHUNK_SIZE);
        }
        if (rest != 0)
        {
            gzwrite(fileHande, reinterpret_cast<const char*>(t) + chunks * CHUNK_SIZE, rest);
        }
    }

    template<typename T>
    bool read(T& t)
    {
        return gzread(fileHande, reinterpret_cast<char*>(&t), sizeof(T)) == sizeof(T);
    }

    template<typename T>
    bool readPtr(T* t, uint64_t len)
    {
        const uint64_t chunks = (sizeof(T) * len) / CHUNK_SIZE;
        const uint64_t rest = (sizeof(T) * len) % CHUNK_SIZE;

        for (uint64_t i = 0; i < chunks; ++i)
        {
            if((uint64_t)gzread(fileHande, reinterpret_cast<char*>(t) + i * CHUNK_SIZE, CHUNK_SIZE) != CHUNK_SIZE)
            {
                return false;
            }
        }

        return (rest == 0) || ((uint64_t)gzread(fileHande, reinterpret_cast<char*>(t) + chunks * CHUNK_SIZE, rest) == rest);
    }
};

template<>
void ZLibFile::write<std::string>(const std::string& s);

template<>
bool ZLibFile::read<std::string>(std::string& s);
