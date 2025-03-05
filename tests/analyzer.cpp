#include <cstdio>
#include <string_view>
#include <string>
#include <fstream>
#include <sstream>
#include <iostream>
#include <format>
#include <alloca.h>

#include "elog.hpp"
#include "../elog.cpp"

namespace Log
{
    void errorStoringArgumentsFor(const char* format) { printf("ERROR: %s\n", format); }
}

int help(const char * progName)
{
    printf("%s : Log buffer analyzer. This tool is used to analyze the memory dump of a log buffer to display what byte does what\n", progName);
    printf("Usage:\n");
    printf("\t%s memoryDumpFile\n", progName);
    printf("\t --help or -h: Display this message\n");
    return 1;
}

std::string readFile(const char * path)
{
    std::ifstream t(path);
    std::stringstream buffer;
    buffer << t.rdbuf();
    return buffer.str();
}

void dualHexDump(const std::string & source, const std::string & decoded)
{
//    if (source.size()*2 != decoded.size()) return;
    // Dump the hexdump of the source and the decoded value
    std::string hexdumpEven, hexdumpOdd, hexdumpSrc;
    for (std::size_t i = 0; i < source.size(); i++)
    {
        if ((i % 64) == 0)
        {
            if (i) printf("%s\n%s\n%s\n\n", hexdumpEven.c_str(), hexdumpSrc.c_str(), hexdumpOdd.c_str());
            hexdumpEven = std::format("{:08x} ", i);
            hexdumpSrc = std::format("{:08x} ", i);
            hexdumpOdd = std::format("{:08x} ", i);

        }

        hexdumpEven += std::format("{:02X} ", (unsigned char)source[i]);
        hexdumpSrc += std::format("{}  ", (char)(source[i] < 32 || source[i] >= 127 ? '.' : source[i]));
        hexdumpOdd += std::format("{}{}{}", (char)decoded[i*3+0], (char)decoded[i*3+1], (char)decoded[i*3+2]);
    }
    printf("%s\n%s\n%s\n\n", hexdumpEven.c_str(), hexdumpSrc.c_str(), hexdumpOdd.c_str());
}

int main(int argc, const char * argv[])
{
    if (argc < 2 || std::string_view(argv[1]) == "-h" || std::string_view(argv[1]) == "--help") return help(argv[0]);

    std::string f = readFile(argv[1]);
    if (!f.size()) return fprintf(stderr, "Invalid given file %s or empty\n", argv[1]);

    std::string decoded;
    // Decode the given file now
    const uint8 * buffer = (const uint8*)f.c_str();
    for (std::size_t i = 0; i < f.size();)
    {
        Log::LogItem * log = (Log::LogItem*)&buffer[i];
        // Here we need to decode the log item flags
        std::string item = "L";
        if (log->FileDump)      item += "f";
        if (log->LineDump)      item += "l";
        if (log->MaskType == 3) item += "m";
        if (log->Repeat)      { item += log->Param ? "p" : "r"; }
        for (std::size_t j = item.size(); j < sizeof(*log) * 3; j++) item += " ";

        i += sizeof(*log);
        // Then read the StoreLogSize
        StoreLogSizeType logSize = *(StoreLogSizeType*)&buffer[i];
        item += std::format("{:02X} ", logSize);
        i += sizeof(logSize);

        // Read the file if present
        if (log->FileDump) {
            uint8 size = buffer[i];
            uint8 used = Log::encodedLen(size);
            item += [](uint8 used) { std::string a = "FF "; for (uint8 i = 1; i < used; i++) a += "FF "; return a; }(used);
            i += used;
            logSize -= used;
        }
        if (log->LineDump) {
            uint64 value = 0;
            uint8 used = Log::decode_u64(&buffer[i], value);
            if (used == 1 && value >= 100)
                item += "LL ";
            else item += std::format("L{:<{}}", value, used * 3 - 1);
            i += used;
            logSize -= used;
        }
        if (log->MaskType == 3) {
            uint64 value = 0;
            uint8 used = Log::decode_u64(&buffer[i], value);
            item += std::format("M{:0{}X}", value, used * 3 - 1);
            i += used;
            logSize -= used;
        }
        // Read the parameters (right now, it's impossible, so let's dump those as unknown)
        for (std::size_t j = 0; j < logSize; j++) item += "PP ";
        i += logSize;
        if (log->Repeat) {
            if (log->Param) {
                // Read parameters size first
                StoreLogSizeType count = *(StoreLogSizeType*)&buffer[i];
                i += sizeof(count);
                item += "RR ";
                for (std::size_t j = 0; j < count; j++) item += "PP ";
                i += count;
            } else {
                // Read the count
                StoreLogSizeType count = *(StoreLogSizeType*)&buffer[i];
                item += "CC ";
                i += sizeof(count);
            }
        }

        decoded += item;
    }


    dualHexDump(f, decoded);


    return 0;
}