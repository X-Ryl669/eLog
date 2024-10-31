# eLog
C++ Log engine for embedded system

## What is it ?

This is a log engine for an embedded system without direct output or access for storing its logs.
It is used to store logs in a buffer that can be retrieve later on (for example when communicating).

## Why use this ?

On Internet Of Things gizmo, the usual behavior is to sleep to save power as network is usually
a sparse availability.

In that case, either the gizmo doesn't save its logs (meaning that debugging it is painful) since it'll not be able to transmit it.
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
4. Built to be completely compatible with printf like format specified (unlike std::fmt that accept any type). This limits the number of types to save in the buffer and hence the code size.
5. The log encoding and saving doesn't use heap memory. Decoding code only use heap when encoutering runtime string, since strings aren't saved with null terminating byte in the log buffer.
6. The library is small size (less than 1000 line of code), so it's possible to embed everywhere. On amd64 system, it compiles to only 7KB of code or 6.5KB of code if not including the decoding code.

### Cons
This library is still using memory to save the logs. But unlike the usual storage of `const char array[]`, this requires less memory so it's possible to fit this buffer
in special RAM area that is kept powered on (many microcontroller have such, size limited, memory that can be retained while sleeping).

In order to achieve printf compatibility and limit the size of the library, the library dumping code (if built in), will use snprintf to generate the output string.
Some microcontroller includes snprintf in their ROM code, so it's a zero cost downside, but some don't (in that case, the library will depend on a snprintf implementation).

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
        Network = 0x0004,
        Power   = 0x0008,
        ADC     = 0x0010,
        Nuclear = 0x0020,
    };

    // Tell the log engine what module you want to store
    Log::logMask = Log::CustomMask | Network | Power | ADC ;
    // Then use log with modules
    logm(Nuclear, "This log will not be saved, since it doesn't match the mask: runtime to launch: %d", 0);
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
                printf("[%u] %s\n", str, mask);
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

## Configuration

This library depends on few configurable variable. Those are:

### RingBuffer size
This is the size, in bytes, of the circular buffer for storing log items. Usually, 512 bytes is enough to store ~2000 lines of logs, which might be more than enough for your application. It must be a power of 2 (since the ring buffer use the & operator as a modulo to avoid a division)
```cpp
#define LogRingBufferSize 512
```

### Deleting old logs when full

By default, when the ring buffer is full, the oldest log items are *deleted* (conceptually, no heap is involved here), until there's enough space to store the new log. This means that you'll keep the latest logs in the buffer.

However, this implies some code to parse the log's format, in order to extract the arguments that were saved in the buffer. If you are really constrainted by the library binary's size in flash, you might want to disable this feature and simply let the system fails to store new logs when it's full.

NOTE: If you consume the logs with `dumpLog`, the old logs are *deleted*, so if you application has a way to dump its log periodically, this might be interesting as well.

```cpp
#define DeleteOldLogsWhenFull 1
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