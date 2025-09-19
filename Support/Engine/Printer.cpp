#include "Printer.h"

#include <iostream>

#include "spdlog/spdlog.h"

static std::string spaces = "                                                                               ";

void Printer::show(void)
{
    if (_size == 0)
    {
        std::cout << "\r" << _header << ": " << _message << spaces << std::flush;
        _force = false;
        return;
    }

    if (_force || 100 *  _lastProgress / _size <= 100 * _progress / _size)
    {
        std::cout << "\r" << _header << " " << 100 * _progress / _size << "%: " << _message << spaces << "\r" << std::flush;
        _lastProgress = _progress;
        _force = false;
    }
}

void Printer::init(const std::string& header, size_t size)
{
    _header = header;
    _size = size;
    _lastProgress = 0;
    _progress = 0;
    _force = true;
    show();
}

void Printer::incProgress(size_t inc)
{
    _progress += inc;
    show();
}

void Printer::print(const std::string& message)
{
    spdlog::debug(message);

    _message = message;
    _force = true;
    show();
}
