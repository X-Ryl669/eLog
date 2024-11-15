#include <cstdio>

#define DeleteOldLogsWhenFull 1
#include "log.hpp"

namespace Log { void errorStoringArgumentsFor(const char* format) { printf("ERROR: %s\n", format); }}

template<typename T>
void dumpType()
{
    printf("%s\n", std::source_location::current().function_name());
}

int main()
{
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

    log("This is a sample log");
    logf("This is another log with %d arguments", 2);
    logfl("Test: %c-%p-%08X something %.*s for %g", '-', "something", 0xDEADBEEF, 7, "strangely", 3.1415926535f);

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
    while (CompileTime::dumpLog([&combinedSize](const char * str, uint32 mask) { combinedSize += strlen(str)+1; printf("%s\n", str); })) {}

    printf("Log compression size: %u/%u = %.2f%%\n", storedLogSize, combinedSize, (float)storedLogSize * 100 / combinedSize);


    // Test wrapping up the circular buffer now
    uint32 iter = 0;
    while (Log::logBuffer.fetchWritePos() >= Log::logBuffer.fetchReadPos())
    {
        log("Sample log for iteration %u", iter++);
    }


#if DeleteOldLogsWhenFull == 1
 //   CompileTime::extractFirstLog();
#endif

    combinedSize = 0;
    storedLogSize = Log::logBuffer.getSize();
    while (CompileTime::dumpLog([&combinedSize](const char * str, uint32 mask) { combinedSize += strlen(str)+1; printf("%s\n", str); })) {}

    printf("Log compression size: %u/%u = %.2f%%\n", storedLogSize, combinedSize, (float)storedLogSize * 100 / combinedSize);

    return 0;
}