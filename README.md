# eLog
C++ Log engine for embedded system

## What is it ?

This is a log engine for an embedded system without direct output or access for storing its logs.
It is used to store logs in a buffer that can be retrieved later on (for example when communicating).

## Why use this ?

On Internet Of Things gizmo, the usual behavior is to sleep to save power as network is usually
a sparse availability.

In that case, either the gizmo doesn't save its logs (meaning that debugging is painful) since it'll not be able to transmit it.
Either the gizmo saves its log (in memory or flash).

Flash write cycles are limited, so only the very important logs will consume the precious cycles, making debugging still hard to perform.
Saving the logs in memory, however, requires maintaining power of the RAM, which is usually hard to achieve on battery powered system.

Logs also take a lot of space while RAM size is limited on these systems.

### Pros
This repository provides an alternative for this issue. Logs using this library will be saved in a ring buffer (a kind of smart circular buffer).
Instead of being saved as a human readable format (understand: UTF8 string), the logs are efficiently and smartly encoded (like a virtual machine for logs):
1. Strings in ROM/Flash (like the format specifier in printf) aren't saved in the buffer, only their pointer. Dynamic (runtime) strings are saved in the buffer.
2. Arguments are length encoded (so small number only takes a single byte, instead of 4 for a 32 bits integer)
3. Format specifier is checked at compile time (unlike printf and other), so a wrong format won't compile, instead of causing a runtime crash
3. Decoding code can be avoided if it can be done by the log consumer (for example, on a web frontend), thus log saving is size limited and fast
4. Built to be completely compatible with printf like format specifier (unlike std::fmt that accept any type). This limits the number of types to save in the buffer and hence the code size.
5. The log encoding and saving doesn't use heap memory. Decoding code only use heap when encoutering runtime string, since strings aren't saved with null terminating byte in the log buffer.
6. The library is small size (less than 1000 line of code), so it's possible to embed everywhere. On amd64 system, it compiles to only 7KB of code or 6.5KB of code if not including the decoding code.

### Cons
This library is still using memory to save the logs. But unlike the usual storage of `const char array[]`, this requires less memory so it's possible to fit this buffer
in special RAM area that is kept powered on (many microcontroller have such, size limited, memory that can be retained while sleeping).

In order to achieve printf compatibility and limit the size of the library, the library dumping code (if built in), will use `snprintf` to generate the output string.
Some microcontroller includes `snprintf` in their ROM code, so it's a zero cost downside, but some don't (in that case, the library will depend on a `snprintf` implementation supporting the format you're using).

The format argument of the log being compile time parsed, it's not possible to generate this format argument dynamically. This is rarely the case in practice.

This library use C++20 (might work with C++17 with some adjustments, patch welcome). So any 4 year old cross-compiler will work.

## Example usage

This library is used like printf is used (or many Log library).
### Include the right header
Add this to the top of the file requiring logging
```cpp
#include "log.hpp"
```

### Save some logs
Use like you are used to:
```cpp
int main()
{
    // Example with simple log
    log("Booting with battery voltage: %.1fV", getBatteryVoltage());

    // Save the filename and only the filename, not the full path
    logf("Starting up!");

    // Save both the filename and the line number
    logfl("This is very embarassing moment, random value is %d", rand());

    // Use a mask so you can enable and disable log at runtime
    // So first, let's define some module your application is using
    enum MyApplicationModule
    {
        Network = 0x0004,  // Don't use bit 0 and bit 1, they are reserved in the library
        Power   = 0x0008,
        ADC     = 0x0010,
        Nuclear = 0x0020,
    };

    // Tell the log engine what module you want to store
    Log::logMask = Log::CustomMask | Network | Power | ADC ;
    // Then use log with modules
    logm(Nuclear, "This log will not be saved, since it doesn't match the mask");
    // This log will be saved (Error and Warning logs are always saved)
    logm(Log::Error, "Sorry, it's too late to cancel the launch, %s", "Adam");
    // This log will be saved too, it matches your own mask, with file and line number
    logflm(Power | ADC, "Not enough power to ignite the engine: %.*s", 3, "Bad luck");
}
```

### Retrieving the logs

To retrieve the logs, you'll need to give the library a callback function (usually a lambda) that'll be called for each log (if run in a loop), like this:
```cpp
int dumpLogsToSocket(int socket)
{
    while(CompileTime::dumpLog(
                [&socket](const char * str, uint32 mask)
                {
                    return send(socket, str, strlen(str), 0) == strlen(str);
                    // Returning false here stops the dump and exit the loop.
                    // The log that failed dumping isn't consumed and will be available
                    // on the next dumpLog call.
                })
         ) {}
}

int dumpLogsOnUART()
{
    while(CompileTime::dumpLog(
            [](const char * str, uint32 mask)
            {
                printf("[%u] %s\n", mask, str);
                // You can also return void if you don't care about keeping
                // the logs on error
            })
        ) {}
}

int main()
{
    // ... after the code from the previous section
    dumpLogsOnUART();
}
```

### Error handling

With the magic of C++, you can now have this:
```cpp
int main(...)
{
    logf("We know that the area of a circle of radius %gmm is %gmm2 since %.*s", 3, 3.1415 * 3 * 3, "Pythagorus");
}

$ make
In file included from main.cpp:4:
../include/log.hpp: In instantiation of 'void CompileTime::LogFormatter<string>::storeArguments(Args&& ...) [with Args = {int, double, const char*}; auto string = CompileTime::str<69>{"We know that the area of a circle of radius %gmm is %gmm2 since %.*s"}]':
../include/log.hpp:954:147:   required from 'CompileTime::LogFormatter<string>::LogFormatter(const CompileTime::sourceloc&, Args ...) [with Args = {int, double, const char*}; auto string = CompileTime::str<69>{"We know that the area of a circle of radius %gmm is %gmm2 since %.*s"}; CompileTime::sourceloc = std::source_location]'
  954 |         LogFormatter(const sourceloc & loc, Args... args) : LogItemSaver((const char*)string, Log::LogMask::Default, false, &loc) { storeArguments(std::forward<Args>(args)...); }
      |                                                                                                                                     ~~~~~~~~~~~~~~^~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main.cpp:70:5:   required from here
 1069 | #define logf(fmt, ...)              CompileTime::LogFormatter<CompileTime::str{fmt}>{CompileTime::sourceloc::current(), __VA_ARGS__}
      |                                                                                                                                    ^
../include/log.hpp:933:39: error: static assertion failed: The number of arguments in the format specifier doesn't match the number of arguments passed in
  933 |             static_assert(expArgCount == sizeof...(Args), "The number of arguments in the format specifier doesn't match the number of arguments passed in");
      |                           ~~~~~~~~~~~~^~~~~~~~~~~~~~~~~~
../include/log.hpp:933:39: note: the comparison reduces to '(4 == 3)'
../include/log.hpp:934:32: error: static assertion failed: The expected arguments doesn't match those in the format specifier
  934 |             static_assert(std::is_same_v<std::decay_t<decltype(argList)>, decltype(SpecifiersTable<N>::getLooselyTypedArguments(impl_string, std::make_index_sequence<N>{}))>, "The expected arguments doesn't match those in the format specifier");
      |                           ~~~~~^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
../include/log.hpp:934:32: note: 'std::is_same_v<CompileTime::TypeList<int, double, void*>, CompileTime::TypeList<double, double, int, void*> >' evaluates to false
```

The error message from the compiler give the reason why it failed, the number of argument isn't good (expecting 4 arguments, 3 given) and the argument type isn't matching either (the first expected argument is an double, a int was given). While the former error is easy to spot, the latter would have been silently accepted with printf and the resulting code would have **crashed** at runtime.

## Configuration

This library depends on few configurable variable. Those are:

### RingBuffer size
This is the size, in bytes, of the circular buffer for storing log items. Usually, 512 bytes is enough to store the last ~100 lines of logs on a 32 bits system, which might be more than enough for your application. It must be a power of 2 (since the ring buffer use `& (size-1)` operation as a modulo to avoid a division)
```cpp
#define LogRingBufferSize 512
```

### Deleting old logs when full

By default, when the ring buffer is full, the oldest log items are *deleted* (conceptually, no heap is involved here), until there's enough space to store the new log. This means that you'll keep the latest logs in the buffer.

However, this implies some code to parse the log's format, in order to extract the arguments that were saved in the buffer. If you are really constrainted by the library binary's size in flash, you might want to disable this feature and simply let the system fails to store new logs when it's full.

**NOTE**: If you consume the logs with `dumpLog`, the old logs are *deleted*, so if you application has a way to dump its log periodically, this might be enough.

```cpp
#define DeleteOldLogsWhenFull 1
```

### Log storage strategy

By default, the log header doesn't contain the stored size for the log and its arguments. This saves memory in the ring buffer, but when deleting logs, it means parsing the log format line to skip the arguments to find the next log position (so it has a cost in binary code size).

If defined to a unsigned type, it will store the size for the log and its arguments after the header allowing to skip the format line parsing (binary code saving) at the cost of the given type storage in memory.

Default to undefined

**NOTE**: If the log's parameters are larger than what can be stored in this type, this will
      likely trigger the failure reporting feature (see below). In that case, the matching log will not be stored in the buffer. Typically, a uint8 will allow to store up to 256 bytes of parameters (the log format string isn't counted in this storage). A uint16 allows up to 65536 bytes, althrough it doesn't make real sense since that would mean a very huge parameter list.

```cpp
#define StoreLogSizeType           uint8
```

### Failure reporting mode
If you've disabled the previous feature (or if the size of the arguments is larger than the log's ring buffer size), the library can fail to store the arguments for a log in the buffer.
In that case, it can either throw a `Log::Exception(const char * format)` on error, or call a user-provided function with the following signature: `Log::void errorStoringArgumentsFor(const char* format)`

```cpp
#define ThrowOnError            0

namespace Log {
    void errorStoringArgumentsFor(const char* format) {
        printf("ERROR: while storing arguments for '%s'\n", format);
    }
}
```

### Using log callback function
If you need to capture the logs at the time they are called, before they are saved in the buffer, for example for outputing them on a serial link, you can define the `UseLogCallback` macro.

You will need to provide a `LogCallback` compatible function that will be called for each log passing the mask. This adds 1.1kB of binary size on the test program.

Default to undefined

```cpp
#define UseLogCallback

void LogCallbackImpl(const char * file, const int line, const uint32 mask, const char * format, va_list args) { ... }

// Start of your main:
Log::LogCallback = LogCallbackImpl;
```
