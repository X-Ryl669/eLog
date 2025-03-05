#include <cstdio>
#include <source_location>
#include <alloca.h>
#include <fstream>

#define DeleteOldLogsWhenFull 1
#define UseLogCallback
#include "elog.hpp"
#include "../elog.cpp"

namespace Log
{
    void errorStoringArgumentsFor(const char* format) { printf("ERROR: %s\n", format); }
    void LogCallbackImpl(const char * file, const int line, const uint32 mask, const char * format, va_list args)
    {
        printf("%s(%d)[%u]:", file, line, mask);
        vprintf(format, args);
        printf("\n");
    }

}

template<typename T>
void dumpType()
{
    printf("%s\n", std::source_location::current().function_name());
}

int main()
{
    Log::LogCallback = Log::LogCallbackImpl;

    // Test encoding and decoding
    {
        uint8 buffer[9] = {0};
        int64 i = 97;
        uint8 used = Log::encode_i64(buffer, i);
        uint64 v = 0;
        Log::decode_u64(buffer, v);
        printf("enc: %llu, dec: %llu\n", i, v);
    }

    /*
    constexpr auto s = str{"something %d%% %.*s %.Lf"};
    constexpr std::size_t c = countArguments(s);
    printf("%d percents in s\n", c);

    constexpr auto st = str_ref<s>{};
    constexpr auto argTable = SpecifiersTable<c>(st);
    printf("ArgTable 0:%p,%p, 1:%p,%p 2:%p,%p\n", argTable.table[0].save, argTable.table[0].load, argTable.table[1].save, argTable.table[1].load, argTable.table[2].save, argTable.table[2].load);

    printf("Subtype0: %c, Specifier0:%c, Subtype1: %c, Specifier1: %c\n", getIthSubtype(0, s), getIthSpecifier(0, s), getIthSubtype(1, s), getIthSpecifier(1, s));
    for (size_t i = 0; i < c; i++)
        printf("%u: iThSpecifier:%s\n", i, nextIthSpecifier(i, s));

    dumpType<decltype(SpecifiersTable<c>::getLooselyTypedArguments(st, std::make_index_sequence<c>{}))>();
    int i = 4;
     auto u = LogFormatter<str{"something very dumb %d"}>{i};
     */

    elog("This is a sample %.3fV and %.2f%%", 3.23, 66.214);
    elog("This %% is %.3f %% abc", 1.0);
    elog("This %%");
    elog("This %%%%");


    elog("This is a sample log");
    elogf("This is another log with %d arguments", 2);
    elogfl("Test: %c-%p-%08X something %.*s for %g", '-', "something", 0xDEADBEEF, 7, "strangely", 3.1415926535f);



    constexpr const char test[] = "Test: %c-%p-%08X something %.*s for %g";
    const char * testrt = "Test: %c-%p-%08X something %.*s for %g";
    std::size_t specCount = CompileTime::countSpecifiers(testrt);
    for (auto i = 0; i < 5; i++)
    {
        CompileTime::SpecifierType specifier = CompileTime::readIthSpecifier(i, test);
        char spec[64] = {'%'};
        if (!specifier) printf("Failed reading %u specifier\n", i);
        else printf("%u: S:%c, SS:%c, SSS:%c =>%s<=\n", i, specifier.first, specifier.second, specifier.specifier, spec);
    }
    printf("Arguments: %u\n", CompileTime::countArguments(test));

    uint32 storedLogSize = Log::logBuffer.getSize();

    printf("Log buffer contains %u bytes:\n", Log::logBuffer.getSize());
    for (auto i = 0; i < Log::logBuffer.getSize(); i++)
    {
        printf("%02X ",Log::logBuffer.buffer[(Log::logBuffer.r + i) & Log::logBuffer.sm1]);
        if ((i & 15 == 0)) printf("\n");
    }
    printf("\n");

    uint32 combinedSize = 0;
    while (CompileTime::dumpLog([&combinedSize](const char * str, uint32 mask, uint32 count) { combinedSize += strlen(str)+1; printf("%s (%d times)\n", str, count); })) {}

    printf("Log compression size: %u/%u = %.2f%%\n", storedLogSize, combinedSize, (float)storedLogSize * 100 / combinedSize);


    // Test wrapping up the circular buffer now
    uint32 iter = 0;
    while (Log::logBuffer.fetchWritePos() >= Log::logBuffer.fetchReadPos())
    {
        elog("Sample log for iteration %u", iter++);
    }
    for (auto i = 0; i < 3; i++)
        elogflm(Log::Warning, "GET /%u", iter);

    // Save log buffer here to test decoding in the analyzer
    {
        std::ofstream o("logbuffer.dump", std::fstream::out | std::fstream::binary);
        const uint8 * head, * tail;
        uint32 headS = 0, tailS = 0, r = Log::logBuffer.fetchReadPos();

        if (Log::logBuffer.load(Log::logBuffer.getSize(), head, headS, tail, tailS))
        {
            o.write((const char*)head, headS);
            o.write((const char*)tail, tailS);
            Log::logBuffer.rollback(r);
        }
    }

#if DeleteOldLogsWhenFull == 1
 //   CompileTime::extractFirstLog();
#endif

    combinedSize = 0;
    storedLogSize = Log::logBuffer.getSize();
    while (CompileTime::dumpLog([&combinedSize](const char * str, uint32 mask, int count) { combinedSize += strlen(str)+1; printf("%s (%d times) %s\n", str, count < 0 ? -count : count, count < 0 ? "repeated" : ""); })) {}

    printf("Log compression size: %u/%u = %.2f%%\n", storedLogSize, combinedSize, (float)storedLogSize * 100 / combinedSize);

    return 0;
}
