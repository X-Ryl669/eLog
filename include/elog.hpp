#pragma once
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cwctype>
#include <functional>
#include <type_traits>
#include <cstring>
#include <cstdarg>

#include "eLogConfig.hpp"

typedef std::uint32_t   uint32;
typedef std::uint64_t   uint64;
typedef std::int64_t    int64;
typedef std::uint8_t    uint8;
typedef std::uint16_t   uint16;



/** Define the size of the log ring buffer in bytes (must be a power of 2)  */
#ifndef LogRingBufferSize
    #define LogRingBufferSize       512
#endif
/** Define the strategy to use when the ring buffer is full.
    By default, it deletes the oldest logs (like a FIFO), but that takes more binary size than doing nothing (in that case it can't store more logs) */
#ifndef DeleteOldLogsWhenFull
    #define DeleteOldLogsWhenFull   1
#endif
/** Define a strategy for log storage.
    By default, the log header doesn't contain the stored size for the log and its arguments. This saves memory in the ring buffer, but when
    deleting logs, it means parsing the log format line to skip the arguments to find the next log position (so it has a cost in binary code size).
    If defined to a unsigned type, it will store the size for the log and its arguments after the header allowing to skip the format line parsing (binary code saving)
    at the cost of the given type storage in memory. */
#ifdef DefineStoreLogSizeType
    #define StoreLogSizeType        uint8
#endif

#if defined(UseLogCompression) && !defined(StoreLogSizeType)
    #error "Enabling log compression requires having a stored log size type"
#endif

/** Define the error strategy when a log can't be saved in the log buffer.
    By default, it calls a function in Log namespace with signature "void errorStoringArgumentsFor(const char* format)" is called
    If set to 1, an exception Log::Exception(const char* format) is thrown instead */
#ifndef ThrowOnError
    #define ThrowOnError            0
#endif

/** The callback function to use when logging for immediate output of the log, using vnprintf wrapper */
#ifdef UseLogCallback
namespace Log { extern void (*LogCallback)(const char * file, const int line, const uint32 mask, const char * format, va_list args); }
#endif

#if INTPTR_MAX == INT32_MAX
    // On 32 bit system, we want to save a word while storing data into the log item. So we reuse some bits from the const char * pointer for storage
    // In order to do so, we have to subtract any static offset from the pointer so that high order bits are available for storage
    // This methods seems to work everywhere, but I'm not sure it's the best.
    #if !defined(CONFIG_IDF_TARGET_ESP32C6) && !defined(CONFIG_IDF_TARGET_ESP32C3) && !defined(CONFIG_IDF_TARGET_ESP32C2) && !defined(CONFIG_IDF_TARGET_ESP32S3) && !defined(CONFIG_IDF_TARGET_ESP32S2) && !defined (CONFIG_IDF_TARGET_ESP32)
    extern "C" {
        /* these are in no header file, and on some
            systems they have a _ prepended
            These symbols have to be typed to keep the compiler happy
            Also check out brk() and sbrk() for information
            about heap */
        extern char  etext, edata, end;
    }
    #endif
#endif

#ifndef BackupMemory
    #define BackupMemory
#endif

#if DeleteOldLogsWhenFull == 1
namespace CompileTime { bool extractFirstLog(); }
#endif

#if WithoutThreads == 1
namespace Log { struct Mutex {}; struct ScopedLock { ScopedLock(const Mutex &); }; }
#else
namespace Log
{
#if defined(xSemaphoreTake) // FreeRTOS
    struct Mutex {
        void acquire() { xSemaphoreTake(h, portMAX_DELAY); }
        void release() { xSemaphoreGive(h); }
        Mutex() : h(xSemaphoreCreateMutexStatic(&buffer)) {}
        StaticSemaphore_t buffer = {};
        SemaphoreHandle_t h;
    };
#elif defined(__STDCPP_THREADS__) && !defined(HasMutex)   // Anything C++17 or more
    struct Mutex {
        void acquire() { m.lock(); }
        void release() { m.unlock(); }
        std::mutex m;
    };
#endif
    struct ScopedLock {
        ScopedLock(Mutex & m) : m(m) { m.acquire(); }
        ~ScopedLock() { m.release(); }
        Mutex & m;
    };
}
#endif

namespace Log
{
    // Log items are made to be as compact as possible. They fit in a machine word size, and store the information for reconstructing the log (except the arguments)
    // In addition, we also reuse the unused bit to compress (run-length-encode) similar log. In effect, if a log is the same as the previously stored log, instead of
    // storing a new log item with the same parameters, we simply set a bit in the item and store the repeat count after the log (with variable length coding).
    // If the parameters are different, another bit is used and only the new parameters are stored in the ring buffer.
    // These two bits are stored in the low bit of the msg's address (since on many system this address is aligned to the word size)

    // This allow some interesting space saving here:
    // 1. For log with no parameters, it saves a complete LogItem storage (usually 4 or 8 bytes) minus the repeat count (1 byte), for the first repeat,
    //    then it's 4 to 8 bytes per repeated log (more if the log is saved with its file, mask and/or line dump)
    // 2. For a log with different parameters, it saves a complete LogItem storage (usually 4 or 8 bytes), and more if the log is saved with its file, mask and/or line dump
    #pragma pack(push, 1)
    template <std::size_t ptrSize = 8>
    struct LogItemT
    {
        uint32 FileDump : 1;
        uint32 LineDump : 1;
        uint32 MaskType : 2;
#ifdef UseLogCompression
        std::uintptr_t msg : 58;
        uint32 Repeat   : 1;
        uint32 Param    : 1;
#else
        std::uintptr_t msg : 60;
#endif

#ifdef UseLogCompression
        static constexpr std::uintptr_t computeAddress(const char * fmt) { return  ((std::uintptr_t)fmt) & 0x00FFFFFFFFFFFFFCULL; } // On 64 bits system, bits 63-48 are never used on userspace and aligned on 4 bytes at least
        static constexpr const char*    makePointer(const std::uintptr_t fmt) { return  (const char*)fmt; }
#else
        static constexpr std::uintptr_t computeAddress(const char * fmt) { return  ((std::uintptr_t)fmt) & 0x00FFFFFFFFFFFFFFULL; } // On 64 bits system, bits 63-48 are never used on userspace
        static constexpr const char*    makePointer(const std::uintptr_t fmt) { return  (const char*)fmt; }
#endif
        void saveFormat(const char * fmt) { msg = computeAddress(fmt); }
        const char * loadFormat() const { return reinterpret_cast<const char*>(msg); }
        bool isRepeated(const LogItemT & item) const { return msg == item.msg && FileDump == item.FileDump && LineDump == item.LineDump && MaskType == item.MaskType; }
    };

    template <>
    struct LogItemT<4>
    {
        uint32_t FileDump : 1;
        uint32_t LineDump : 1;
        uint32_t MaskType : 2;
#ifdef UseLogCompression
        std::uintptr_t msg : 26;
        uint32 Repeat   : 1;
        uint32 Param    : 1;
#else
        std::uintptr_t msg : 28; // We limit ourselves to only 28 bits for the pointer, which spans only 256MB range:
                                 // This is likely to fail by default on any usual 32 bit system.
                                 // However, on a microcontroller, 256MB is already a large area and it might fit the whole flash
                                 // The basic idea with this scheme is to store the minimal address seen and the offset from this address
                                 // If it doesn't work, it'll crash
#endif

#if defined(CONFIG_IDF_TARGET_ESP32C6) || defined(CONFIG_IDF_TARGET_ESP32C3) || defined(CONFIG_IDF_TARGET_ESP32C2) || defined(CONFIG_IDF_TARGET_ESP32S3)
        // On ESP32 the flash is is mapped at 0x4200 0000, so there's no point in storing this offset here as it'll always be the same address
        // Also, the low 2 bits are never used for static strings, so we could store value in them too
        static constexpr std::uintptr_t minAddress() { return 0x42000000; }
#elif defined(CONFIG_IDF_TARGET_ESP32S2) || defined (CONFIG_IDF_TARGET_ESP32)
        // On first generation of ESP32 the flash is is mapped at 0x400C 2000, so there's no point in storing this offset here as it'll always be the same address
        // Also, the low 2 bits are never used for static strings, so we could store value in them too
        static constexpr std::uintptr_t minAddress() { return 0x400C2000; }
#else
    #if INTPTR_MAX == INT32_MAX
        static constexpr std::uintptr_t minAddress() { return std::min((std::uintptr_t)&etext, (std::uintptr_t)&edata); }
    #else
        static constexpr std::uintptr_t minAddress() { return 0; }
    #endif
#endif

#ifdef UseLogCompression
        static constexpr std::uintptr_t computeAddress(const char * fmt) { return ((std::uintptr_t)fmt - minAddress()) & 0xFFFFFFFC; }
        static constexpr const char*    makePointer(const std::uintptr_t fmt) { return reinterpret_cast<const char*>(fmt + minAddress()); }
#else
        static constexpr std::uintptr_t computeAddress(const char * fmt) { return (std::uintptr_t)fmt - minAddress(); }
        static constexpr const char*    makePointer(const std::uintptr_t fmt) { return reinterpret_cast<const char*>(fmt + minAddress()); }
#endif
        void saveFormat(const char * fmt) { msg = computeAddress(fmt); }  // On 64 bits system, bits 63-48 are never used on userspace
        const char * loadFormat() const { return makePointer(msg); }
        bool isRepeated(const LogItemT & item) const { return msg == item.msg && FileDump == item.FileDump && LineDump == item.LineDump && MaskType == item.MaskType; }
    };
    // Use the version that's supported on your platform word size
    typedef LogItemT<sizeof(void*)> LogItem;
    #pragma pack(pop)


    // Simple byte swap for 64 bits values. Might not be needed
    inline uint64 byteswap(uint64 val)
    {
        return ((((val) & 0xff00000000000000ull) >> 56) |
                (((val) & 0x00ff000000000000ull) >> 40) |
                (((val) & 0x0000ff0000000000ull) >> 24) |
                (((val) & 0x000000ff00000000ull) >> 8 ) |
                (((val) & 0x00000000ff000000ull) << 8 ) |
                (((val) & 0x0000000000ff0000ull) << 24) |
                (((val) & 0x000000000000ff00ull) << 40) |
                (((val) & 0x00000000000000ffull) << 56));
    }

    /** Using vu128 encoding format for variable length encoding in the buffer. Kudos John Millikin
        Decode the total length for the given header byte (including this byte) */
    uint8 encodedLen(uint8 b);
    /** Using vu128 encoding format for variable length encoding in the buffer. Kudos John Millikin */
    uint8 encode_u64(uint8 buf[9], uint64 x);
    /** Using vu128 encoding format for variable length encoding in the buffer. Kudos John Millikin */
    uint8 decode_u64(const uint8 buf[9], uint64 & x);
    /** Simple zigzag encoding for signed type, to avoid storing a 2 complement negative number that wouldn't compress well */
    uint8 encode_i64(uint8 buf[9], int64 x);
    /** Simple zigzag decoding for signed type, to avoid storing a 2 complement negative number that wouldn't compress well */
    uint8 decode_i64(const uint8 buf[9], int64 & x);
    // Is it really required ? I'm not sure it's worth it
    uint8 encode_f64(uint8 buf[9], double d);
    uint8 decode_f64(uint8 buf[9], double & d);


    /** A Power-Of-2 ring buffer (aka Circular Buffer).
        This is used to store the logs, compressed (the recipe for the log, not the log itself).
        Then this is also used to recreate a textual representation of the log stored inside.

        The idea is to store variable length logs like this:
        LogItem [Opt Filename] [Opt Line as VLC] [Opt Mask] [Optional Argument list] LogItem... */
    template <std::size_t sizePowerOf2>
    struct RingBuffer
    {
        /** The mutex used to protect this structure, manipulated from outside */
        Mutex                           mutex;
        /** Read and write pointer in the ring buffer */
        uint32                          r, w;
#ifdef UseLogCompression
        uint32                          lastLogPos = sizePowerOf2;
#endif
        /** Buffer size minus 1 in bytes */
        static constexpr const uint32   sm1 = sizePowerOf2 - 1;
        /** The buffer to write packets into */
        uint8                           buffer[sizePowerOf2];

        /** Get the consumed size in the buffer */
        inline uint32 getSize() const { return w >= r ? w - r : (sm1 - r + w + 1); }
        /** Get the available size in the buffer */
        inline uint32 freeSize() const { return sm1 - getSize(); }
        /** Fetch the current read position (used to restore the read pointer later on on rollback) */
        inline uint32 fetchReadPos() const { return r; }
        /** Fetch the current read position (used to restore the read pointer later on on rollback) */
        inline uint32 fetchWritePos() const { return w; }
        /** Rollback with the saved read position */
        inline void rollback(const uint32 readPos) { if (readPos >= sm1) return; r = readPos; }
        /** Rollback the saved write position */
        inline void rollbackWrite(const uint32 writePos) { if (writePos >= sm1) return; w = writePos; }
        /** Check if the buffer is full (and, if configured to do so, clean the buffer until there's enough free space) */
        bool canFit(const uint32 size)
        {
            if (size > sm1) return false; // Can't store the data anyway
            if (freeSize() >= size) return true;
#if DeleteOldLogsWhenFull == 1
            // Clean the logs recursively until we have enough space
            while (freeSize() < size)
                // The idea, here is to clean log one by one until there's enough space for the new log.
                // In order to do so, we have to parse the log using the CompileTime's namespace function.
                // But unlike when generating the log, we only care about extracting specifiers type so we can
                // call or mimick the load function for the arguments matching the specifiers
                if (!CompileTime::extractFirstLog()) return false;

            return true;
#endif
            return false;
        }

        /** Add data to this buffer (no allocation is done at this time) */
        bool save(const uint8 * packet, uint32 size)
        {
            // Check we can fit the packet
            if (!canFit(size)) return false;

            const uint32 part1 = std::min(size, sm1 - w + 1);
            const uint32 part2 = size - part1;

            memcpy((buffer + w), packet, part1);
            memcpy((buffer), packet + part1, part2);

            w = (w + size) & sm1;
            return true;
        }
        /** Extract data from the buffer.
            We can skip one copy to rebuilt a contiguous packet by simply returning
            the two part in the ring buffer and let the application send them successively.
            From the receiving side, they'll be received as one contiguous packet. */
        bool load(uint32 size, const uint8 *& packetHead, uint32 & sizeHead, const uint8 *& packetTail,  uint32 & sizeTail)
        {
            if (getSize() < size) return false;
            packetHead = buffer + r;
            sizeHead = std::min(size, (sm1 - r + 1));
            sizeTail = size - sizeHead;
            packetTail = buffer;
            r = (r + size) & sm1;
            return true;
        }

        /** Peek a byte from the buffer. Doesn't advance the read pointer */
        bool peek(uint8 & x)
        {
            if (!getSize()) return false;
            x = buffer[r];
            return true;
        }

        /** The generic save function for the logs */
        template <typename T>
        bool saveType(const T val)
        {
            return save((const uint8*)&val, sizeof(val));
        }
        /** Overload for unsigned integral types to use VLC encoding*/
        bool save(const std::unsigned_integral auto i)
        {
            uint8 buf[9] = {};
            uint8 size = encode_u64(buf, i);
            return save(buf, size);
        }
        /** Overload for signed integral types to use VLC encoding*/

        bool save(const std::signed_integral auto i)
        {
            uint8 buf[9] = {};
            uint8 size = encode_i64(buf, i);
            return save(buf, size);
        }
        /** Save a string to the buffer
            @param str  A pointer on the C string to save
            @param len  If non zero, contains the actual number of bytes to save (don't include the final NUL)
                        Else, compute the string length from the actual string size. */
        bool saveString(const char * str, std::size_t len = 0)
        {
            if (!len) len = strlen(str);
            uint8 c = 0;
            return save((const uint8*)str, len) && save(&c, 1);
        }
        /** Save a pointer without VLC */
        bool save(const void * ptr)
        {
            std::uintptr_t p = (std::uintptr_t)(ptr);
            return save((const uint8*)&p, sizeof(p));
        }
        bool save(const char * ptr)
        {
            static_assert(sizeof(ptr) == sizeof(void*), "You shouldn't use this to store a string, instead use saveString");
            return false;
        }
        /** Save a float or a double */
        bool save(const double i)       { return saveType(i); }
        /** Save a long double */
        bool save(const long double i)  { return saveType(i); }

        /** Generic load a value from the buffer */
        template<typename T>
        bool loadType(T & val)
        {
            const uint8 * head = 0, * tail = 0;
            uint32 sh = 0, st = 0;
            if (!load(sizeof(val), head, sh, tail, st)) return false;
            memcpy((uint8*)&val, head, sh);
            memcpy((uint8*)&val + sh, tail, st);
            return true;
        }
        /** Specialization for unsigned integral to load from VLC */
        bool load(std::unsigned_integral auto & i)
        {
            uint8 buf[9] = {};
            if (!peek(buf[0])) return false;
            const uint8 * head = 0, * tail = 0;
            uint32 sh = 0, st = 0;
            uint8 size = encodedLen(buf[0]);
            if (!load(size, head, sh, tail, st)) return false;
            memcpy(buf, head, sh);
            memcpy(buf + sh, tail, st);
            uint64 res = 0;
            decode_u64(buf, res);
            i = static_cast<std::decay_t<decltype(i)>>(res);
            return true;
        }
        /** Specialization for signed integral to load from VLC */
        bool load(std::signed_integral auto & i)
        {
            uint8 buf[9] = {};
            if (!peek(buf[0])) return false;
            const uint8 * head = 0, * tail = 0;
            uint32 sh = 0, st = 0;
            uint8 size = encodedLen(buf[0]);
            if (!load(size, head, sh, tail, st)) return false;
            memcpy(buf, head, sh);
            memcpy(buf + sh, tail, st);
            int64 res = 0;
            decode_i64(buf, res);
            i = static_cast<std::decay_t<decltype(i)>>(res);
            return true;
        }
        /** Load a string from the buffer
            @param str  If not null, will copy the C string into (including the terminating NUL byte)
            @param len  On output, will be filled with the number of bytes required to load this string, including the terminating NUL byte */
        bool loadString(char * str, std::size_t & len)
        {
            // Find length first
            for (len = 0; ((r + len) & sm1) != w; len++)
                if (buffer[(r+len) & sm1] == 0) break;

            if (((len + r) & sm1) == w) return false;
            len++; // Account for NUL byte
            if (!str) return true;

            for (std::size_t l = 0; l < len; l++)
               str[l] = buffer[(r + l) & sm1];

            r = (r + len) & sm1;
            return true;
        }
        /** Load a pointer without VLC decoding */
        bool load(const void *& ptr)
        {
            std::uintptr_t p = {};
            if (!loadType(p)) return false;
            ptr = (const void*)p;
            return true;
        }
        bool load(const char * ptr)
        {
            static_assert(sizeof(ptr) == sizeof(void*), "You shouldn't use this to load a string, instead use loadString");
            return false;
        }

        bool load(double & i) { return loadType(i); }
        bool load(long double & i) { return loadType(i); }

#if UseLogCompression == 1
        /** Check if the data at position readPos match exactly the given value.
            @return true if it does and increase readPos by the required size in that case for the next value type.
                         else it returns false and doesn't modify readPos */
        bool matchValue(uint32 & readPos, const auto & val)
        {
            uint32 rSave = r;
            r = readPos;
            auto tmp = val;
            if (!load(tmp) || tmp != val) { r = rSave; return false; }
            readPos = r;
            r = rSave;
            return true;
        }
        /** Check if the data at position readPos match exactly the given value.
            @return true if it does and increase readPos by the required size in that case for the next value type.
                         else it returns false and doesn't modify readPos */
        bool matchValue(uint32 & readPos, void * val)
        {
            uint32 rSave = r;
            r = readPos;
            const void * tmp = 0;
            if (!load(tmp) || tmp != val) { r = rSave; return false; }
            readPos = r;
            r = rSave;
            return true;
        }
        /** Check if the data at position readPos match exactly the given value.
            @return true if it does and increase readPos by the required size in that case for the next value type.
                         else it returns false and doesn't modify readPos */
        bool matchValue(uint32 & readPos, const char * val, const std::size_t & len)
        {
            uint32 rSave = r;
            r = readPos;
            std::size_t expLen = 0;
            if (!loadString(nullptr, expLen) || expLen != len) { r = rSave; return false; }
            // Then compare the string itself to match, ignoring the last 0 sentinel that might not be present in the string itself
            if (memcmp(val, &buffer[r], std::min((uint32)len - 1, sm1 - r + 1))) { r = rSave; return false; }
            if (len - 1 > (sm1 - r + 1) && memcmp(&val[sm1 - r + 1], buffer, len - 1 - (sm1 - r + 1))) { r = rSave; return false; }
            readPos = (r + len) & sm1;
            r = rSave;
            return true;
        }

        /** Generic load a value from the buffer */
        template<typename T>
        bool loadTypeAt(const uint32 pos, T & val)
        {
            const uint8 * head = 0, * tail = 0;
            uint32 sh = 0, st = 0;
            uint32 rSave = r; r = pos & sm1;
            if (!load(sizeof(val), head, sh, tail, st)) { r = rSave; return false; }
            r = rSave;
            memcpy((uint8*)&val, head, sh);
            memcpy((uint8*)&val + sh, tail, st);
            return true;
        }

        /** The generic save function for the logs */
        template <typename T>
        bool saveTypeAt(const uint32 pos, const T val)
        {
            uint32 wSave = w; w = pos & sm1;
            bool ret = save((const uint8*)&val, sizeof(val));
            w = wSave;
            return ret;
        }
#endif

#if DeleteOldLogsWhenFull == 1
        inline bool consume(const uint32 s) { if (getSize() <= s) return false; r = (r + s) & sm1; return true; }
        template <typename T>
        bool skipLoad()
        {
            if constexpr(std::is_integral_v<T> || std::is_same_v<T, const void*>)
                return consume(encodedLen(buffer[r]));
            else if constexpr(std::is_same_v<T, double> || std::is_same_v<T, long double>)
                return consume(sizeof(T));
            else
                return false;
        }
#endif

        /** Build the ring buffer */
        RingBuffer() : r(0), w(0)
        {
            static_assert(!(sizePowerOf2 & (sizePowerOf2 - 1)), "Size must be a power of two");
            static_assert(sizePowerOf2 > 32, "A minimum size is required");
        }
    };

    /** The main log buffer that'll store all the log items */
    inline BackupMemory RingBuffer<LogRingBufferSize> logBuffer;

    /** Bitfield of possible mask type, don't change the first 3 items */
    enum LogMask
    {
        Default = 0,
        Error   = 1,
        Warning = 2,
        Other   = 4,
        CustomMask = 3,
        // And so on, adapt to your application, use a bitfield here
    };
    /** The current log mask */
    inline uint32 logMask = 0xFFFFFFFF;

    /** Make sure we can OR with custom enum flags too */
    template <typename T> requires std::is_enum_v<T>
    constexpr inline LogMask operator | (LogMask l, T e) { return (LogMask)((uint32)l | (uint32)e); }


#if ThrowOnError == 1
    struct Exception { const char * format; Exception(const char* format): format(format) {}};
#else
    void errorStoringArgumentsFor(const char * format);
#endif
}

// Compile time operation are stored in the namespace below. You don't need to care about the mess inside this namespace.
namespace CompileTime
{
    /** A Compile time string. This is used to store a char array in a way that the compiler can deal with */
    template <std::size_t N>
    struct str
    {
        static constexpr std::size_t size = N;
        char data[N] = {0};

        constexpr str(const char (&s)[N]) {
            for(std::size_t i = 0; i < N; i++)
            {
                if (!s[i]) break;
                data[i] = s[i];
            }
        }
        template <std::size_t M> constexpr str(const char (&s)[M], std::size_t offset) {
            for(std::size_t i = 0; i < N; i++)
            {
                if (!s[i+offset]) break;
                data[i] = s[i + offset];
            }
        }
        constexpr operator const char*() const { return data; }
    };

    /** Help the compiler deduce the type (with the number of bytes) from the given static array */
    template <std::size_t N> str(const char (&s)[N]) -> str<N>;

    // This is to link a template constexpr to a char array reference that's usable in parsing context
    // This is equivalent to template <typename Type, Type S> to be used as template <typename str<N>, str<N> value>
    template <const auto S>
    struct str_ref {
        constexpr static auto & instance = S;
        constexpr static auto & data = S.data;
    };

    /** Find the last slash in the file path to only extract the filename */
    template <std::size_t N>
    constexpr std::size_t rfind(const char (&data)[N], const char c) {
        for (std::size_t i = N-1;i; i--) if (data[i] == c) return i+1;
        return 0;
    }
    template <auto s, char c = '/'> constexpr auto basename()
    {
        constexpr std::size_t pos = rfind(s.data, c);
        return str<s.size - pos+1>{s.data, pos};
    }


    /** We only care for the expected parameter type (and automatic conversion) here, not the specific formatting rule
        (like uppercase), so all formatting char are compared lowercase.
        List made from here: https://cplusplus.com/reference/cstdio/printf/ */
    enum class Specifier : char
    {
        Unknown              = 0,

        SignedDecInt         = 'd',
        SignedDecIntAlt      = 'i',
        UnsignedDecInt       = 'u',
        UnsignedOctInt       = 'o',
        UnsignedHexInt       = 'x',
        Double               = 'f',
        DoubleExp            = 'e',
        DoubleAuto           = 'g',
        DoubleHex            = 'a',
        Char                 = 'c',
        String               = 's',
        Pointer              = 'p',
    //    Nothing              = 'n', // Not supported in our parsing below
    };

    /** Check if this specifier has a length, like in %*specifier */
    static constexpr bool hasSpecifierLength(Specifier val) { return val != Specifier::Unknown && (char)val <= 'Z'; }
    static constexpr Specifier makeSpecifier(const char val, bool len = false) { return Specifier{len ? (char)(val - 32) : val}; }
    static constexpr Specifier specifierType(const Specifier v) { return Specifier{(char)v <= 'Z' ? (char)((char)v + 32) : (char)v}; }

    // Specifiers array used for fast searching sorted alphabetically so it's possible to search in O(logN) TODO
    static constexpr const char specifiers[] = {
                                                (char)Specifier::DoubleHex      ,
                                                (char)Specifier::Char           ,
                                                (char)Specifier::SignedDecInt   ,
                                                (char)Specifier::DoubleExp      ,
                                                (char)Specifier::Double         ,
                                                (char)Specifier::DoubleAuto     ,
                                                (char)Specifier::SignedDecIntAlt,
                                                (char)Specifier::UnsignedOctInt ,
                                                (char)Specifier::Pointer        ,
                                                (char)Specifier::String         ,
                                                (char)Specifier::UnsignedDecInt ,
                                                (char)Specifier::UnsignedHexInt ,
    };

    // Runtime simplifier for the type extraction
    constexpr static inline Specifier simplify(Specifier s)
    {
        switch(s)
        {
        case Specifier::SignedDecIntAlt: return Specifier::SignedDecInt;
        case Specifier::UnsignedOctInt: return Specifier::UnsignedDecInt;
        case Specifier::UnsignedHexInt: return Specifier::UnsignedDecInt;
        case Specifier::DoubleExp: return Specifier::Double;
        case Specifier::DoubleAuto: return Specifier::Double;
        case Specifier::DoubleHex: return Specifier::Double;
        default: return s;
        }
    }

    // The specified subtype (used for speeding up parsing and matching the actual given type)
    enum class SubType : char
    {
        None              = 0,

        Short             = 'h',
        Long              = 'l',
        IntMax            = 'j',
        Size              = 'z',
        PtrDiff           = 't',
        LongDouble        = 'L',
    };

    static constexpr const char subTypes[] = {
                                                (char)SubType::Short,
                                                (char)SubType::Long,
                                                (char)SubType::IntMax,
                                                (char)SubType::Size,
                                                (char)SubType::PtrDiff,
                                                (char)SubType::LongDouble,
    };

    /** Specifier types tuple like struct to hold them all */
    struct SpecifierType
    {
        SubType first;
        SubType second;
        Specifier specifier;
        constexpr SpecifierType(SubType first = SubType::None, SubType second = SubType::None, Specifier specifier = Specifier::Unknown) : first(first), second(second), specifier(specifier) {}
        constexpr bool hasLength() const { return hasSpecifierLength(specifier); }
        constexpr bool isValid() const { return specifier != Specifier::Unknown; }
        constexpr explicit operator bool() const { return isValid(); }
    };

    struct InvalidSpecifiedType{};
    // Expected type array
    template <SubType sub, SubType sub2, Specifier spec> struct PossibleType { typedef InvalidSpecifiedType Type; };
    // Because I'm lazy with Copy & pasting
    #define DeclareType(t,T,s,X) template <> struct PossibleType<SubType::t, SubType::T, Specifier::s> { typedef X Type; }
        // Default type
        DeclareType(None, None, SignedDecInt, int);
        DeclareType(None, None, UnsignedDecInt, unsigned);
        DeclareType(None, None, Double, double);
        DeclareType(None, None, Char, char);
        DeclareType(None, None, String, char*);
        DeclareType(None, None, Pointer, void*);
        // Subtypes : Signed
        DeclareType(Short, None, SignedDecInt, short int);
        DeclareType(Short, Short, SignedDecInt, signed char);
        DeclareType(Long, None, SignedDecInt, long int);
        DeclareType(Long, Long, SignedDecInt, long long int);
        DeclareType(IntMax, None, SignedDecInt, intmax_t);
        DeclareType(Size, None, SignedDecInt, size_t);
        DeclareType(PtrDiff, None, SignedDecInt, ptrdiff_t);
        // Subtypes : Unsigned
        DeclareType(Short, None, UnsignedDecInt, short unsigned int);
        DeclareType(Short, Short, UnsignedDecInt, unsigned char);
        DeclareType(Long, None, UnsignedDecInt, long unsigned int);
        DeclareType(Long, Long, UnsignedDecInt, long long unsigned int);
        DeclareType(IntMax, None, UnsignedDecInt, uintmax_t);
        DeclareType(Size, None, UnsignedDecInt, size_t);
        DeclareType(PtrDiff, None, UnsignedDecInt, ptrdiff_t);
        // Subtypes : Double
        DeclareType(LongDouble, None, Double, long double);
        // Subtypes : Char and strings
        DeclareType(Long, None, Char, wint_t);
        DeclareType(Long, None, String, wchar_t*);
    #undef DeclareType


    // TypeList meta programming below. You can skip this uglyness, it's quite standard operations for manipulating a list of types at compile time.
    // This section is written to create nightmare for your C++ compiler.

    /** A plain old dumb typelist used to build variadic templates and functions arguments */
    template <typename...Types> struct TypeList{ static constexpr std::size_t size = sizeof...(Types); };

    /** When a specifier has a length, like %.*s, it means that 2 arguments must be consumed for producing this specifier, so let's remember that */
    template<typename T> struct WithLength { typedef TypeList<int, T> Type; };

    template <SubType sub, SubType sub2, Specifier spec>
    constexpr auto getExpectedType()
    {
        if constexpr(hasSpecifierLength(spec)) return typename WithLength<typename PossibleType<sub, sub2, simplify(specifierType(spec))>::Type>::Type{};
        else return typename PossibleType<sub, sub2, simplify(specifierType(spec))>::Type{};
    }
    template <const auto type>
    constexpr auto getExpectedType()
    {
        if constexpr(hasSpecifierLength(type.specifier)) return typename WithLength<typename PossibleType<type.first, type.second, simplify(specifierType(type.specifier))>::Type>::Type{};
        else return typename PossibleType<type.first, type.second, simplify(specifierType(type.specifier))>::Type{};
    }

    /** Decaying the given typelist with std::decay_t, so it's possible to compare given type in the argument list and those declared in the specifiers */
    template <typename...Types> struct DecayedTypeList;
    template <> struct DecayedTypeList<> { using Type = TypeList<>; };
    template <typename Head, typename... Tail> struct DecayedTypeList<Head, Tail...> { using Type = TypeList<std::decay_t<Head>, std::decay_t<Tail>...>; };


    /** Mimic default argument promotion for va_args ( see https://en.cppreference.com/w/cpp/language/variadic_arguments / https://www.gnu.org/software/c-intro-and-ref/manual/html_node/Argument-Promotions.html) */
    template <typename T> struct DefaultArgumentPromotion { typedef T Type; };
    template <> struct DefaultArgumentPromotion<std::nullptr_t> { typedef void * Type; };
    template <typename T> struct DefaultArgumentPromotion<const T *> { typedef void* Type; };
    template <typename T> struct DefaultArgumentPromotion<T *> { typedef void* Type; };
    template <> struct DefaultArgumentPromotion<float> { typedef double Type; };
    template <> struct DefaultArgumentPromotion<bool> { typedef int Type; };
    template <> struct DefaultArgumentPromotion<char> { typedef int Type; };
    template <> struct DefaultArgumentPromotion<short int> { typedef int Type; };
    template <> struct DefaultArgumentPromotion<unsigned char> { typedef int Type; };
    template <> struct DefaultArgumentPromotion<unsigned short int> { typedef int Type; };
    template <typename T, std::size_t N> struct DefaultArgumentPromotion<T (&)[N]> { typedef void * Type; };
    template <typename T, std::size_t N> struct DefaultArgumentPromotion<const T (&)[N]> { typedef void * Type; };

    template <typename T> struct SmartArgumentPromotion { typedef typename DefaultArgumentPromotion<std::decay_t<T>>::Type Type; };
    template <> struct SmartArgumentPromotion<const char *> { typedef const char* Type; };
    template <> struct SmartArgumentPromotion<char *> { typedef char* Type; };
    template <std::size_t N> struct SmartArgumentPromotion<char (&)[N]> { typedef char * Type; };
    template <std::size_t N> struct SmartArgumentPromotion<const char (&)[N]> { typedef char * Type; };


    /** A typelist with decayed and promoted types */
    template <typename...Types> struct DecayedPromotedTypeList;
    template <> struct DecayedPromotedTypeList<> { using Type = TypeList<>; };
    template <typename Head, typename... Tail> struct DecayedPromotedTypeList<Head, Tail...> { using Type = TypeList<typename DefaultArgumentPromotion<std::decay_t<Head>>::Type, typename DefaultArgumentPromotion<std::decay_t<Tail>>::Type...>; };
    template <typename...Types> struct DecayedPromotedTypeList<TypeList<Types...>> { using Type = TypeList<typename DefaultArgumentPromotion<std::decay_t<Types>>::Type...>; };

    template <typename...Types> struct DecayedSmartPromotedTypeList;
    template <> struct DecayedSmartPromotedTypeList<> { using Type = TypeList<>; };
    template <typename Head, typename... Tail> struct DecayedSmartPromotedTypeList<Head, Tail...> { using Type = TypeList<typename SmartArgumentPromotion<std::decay_t<Head>>::Type, typename SmartArgumentPromotion<std::decay_t<Tail>>::Type...>; };

    /** Type list manipulation tools used to transform the complete argument list to the expected argument list */
    // Merge typelists
    template <typename T, typename U> struct Merge { typedef TypeList<T, U> Type; };
    template <typename ... T, typename ... U> struct Merge<TypeList<T...>, TypeList<U...>> { typedef TypeList<T..., U...> Type; };
    template <typename T, typename ... U> struct Merge<T, TypeList<U...>> { typedef TypeList<T, U...> Type; };
    template <typename ... T, typename U> struct Merge<TypeList<T...>, U> { typedef TypeList<T..., U> Type; };
    // Get the N-th type in the list
    template <std::size_t index, typename T, typename... TS> struct NthInner { using Type = typename NthInner<index - 1, TS...>::Type; };
    template <typename T, typename... TS> struct NthInner<0, T, TS...> { using Type = T; };

    template <std::size_t index, typename U> struct Nth {};
    template <std::size_t index, typename ... TS> struct Nth<index, TypeList<TS...> > { using Type = typename NthInner<index, TS...>::Type; };

    // Replace the Nth to Mth type with the given type list
    template<std::size_t O, class...> class wrapper{}; // This is used as a catchall to help the compiler deduce the template's argument
    template <typename T, std::size_t Offset, std::size_t... Is> TypeList<typename Nth<Is + Offset, T>::Type...> Selector(wrapper<Offset, T, std::index_sequence<Is...>>);
    template <std::size_t N, std::size_t M, typename T, typename U> struct Replace {};
    template <std::size_t N, std::size_t M, typename ... T, typename ... U>
    struct Replace<N, M, TypeList<T...>, TypeList<U...>>
    {
        using IndicesBefore = std::make_index_sequence<N>;
        using IndicesAfter = std::make_index_sequence<sizeof...(U)-M>;
        using First = decltype(Selector(wrapper<0, TypeList<U...>, IndicesBefore>{}));
        using Last = decltype(Selector(wrapper<M, TypeList<U...>, IndicesAfter>{}));
        using Type = typename Merge<First, typename Merge<TypeList<T...>, Last>::Type>::Type;
    };
    template <std::size_t N, std::size_t M, typename T, typename ... U>
    struct Replace<N, M, T, TypeList<U...>>
    {
        using IndicesBefore = std::make_index_sequence<N>;
        using IndicesAfter = std::make_index_sequence<sizeof...(U)-M>;
        using First = decltype(Selector(wrapper<0, TypeList<U...>, IndicesBefore>{}));
        using Last = decltype(Selector(wrapper<M, TypeList<U...>, IndicesAfter>{}));
        using Type = typename Merge<First, typename Merge<T, Last>::Type>::Type;
    };
    // A flatten operator, that loops other all types in the type list, merge the resulting "meta" type's Type and returns that
    template <typename... T> struct FlattenInner { using Type = TypeList<>; }; // End recursion
    template <typename... T> struct FlattenInner<TypeList<T...>> { using Type = TypeList<T...>; };
    template <typename... T0, typename... T1, typename... T2> struct FlattenInner<TypeList<T0...>, TypeList<T1...>, T2...> { using Type = typename FlattenInner<TypeList<T0..., T1...>, T2...>::Type; };

    template<typename T> struct Flatten { using Type = TypeList<T>; };
    template<typename... TT> struct Flatten<TypeList<TT...>> { using Type = typename FlattenInner<typename Flatten<TT>::Type...>::Type; };


    static constexpr const char ignoreTable[] = { '.', '*', '+', '-', '#', ' ', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' };
    template <std::size_t N> constexpr bool isIn(const char c, const char (&arr)[N]) { for(auto i = 0; i < N; i++) if (c==arr[i]) return true; return false; }
    template <std::size_t N> constexpr bool isInCaseless(const char c, const char (&arr)[N]) { for(auto i = 0; i < N; i++) if (c==arr[i] || c==(arr[i]-32)) return true; return false; }


    /** Get a pointer to the next specifier.
        This doesn't care for %*d or %.*s like specifier that are viewed as a single specifier */
    constexpr const char * nextSpecifier(const char * str, bool allowEscape = false) {
        for (std::size_t i = 0; str[i]; i++) {
            if (str[i] == '%') {
                if (!str[i+1]) return nullptr;
                if (!allowEscape && str[i+1] == '%') i++; // Ignore %% in the format string here
                else return &str[i+1];
            }
        }
        return nullptr;
    }
    /** Get a pointer to the next i-th specifier.
        This takes care of %*d and %.*s dual specifiers (the first argument is a number and the second the actual specified type).
        So when such specifier is found, the index must be incremented by 2 to reach the next '%' specifier */
    constexpr const char * nextIthSpecifier(std::size_t i, const char * str) {
        do
        {
            str = nextSpecifier(str);
            if (!str) return nullptr;
            const char * s = str;
            char c = *s;
            // Validate specifier
            while (c && isIn(c, ignoreTable)) c = *++s;
            if (!c) return nullptr;

            while (c && isIn(c, subTypes)) c = *++s;
            if (!c) return nullptr;

            if (!isInCaseless(c, specifiers)) return nullptr;

            if (!i) return str;
        } while (i--);
        return nullptr;
    }
    /** Get the subtype for the current specifier */
    constexpr SubType getSubtype(const char * str) {
        if (!str || !isIn(*str, subTypes)) return SubType::None;
        return (SubType)*str;
    }
    /** Extract all information about the ith specifier */
    constexpr SpecifierType readIthSpecifier(std::size_t i, const char * str)
    {
        SpecifierType specifier;
        str = nextIthSpecifier(i, str);
        if (!str) return specifier;
        bool sized = false;
        char c = *str;
        while (c && isIn(c, ignoreTable))
        {
            if (c == '*') sized = true;
            c = *++str;
        }
        if (!c) return specifier; // Shouldn't be short
        if ((specifier.first = getSubtype(str)) != SubType::None) {
            str++;
            if ((specifier.second = getSubtype(str)) != SubType::None) str++;
        }
        if (!isInCaseless(*str, specifiers)) return specifier;
        specifier.specifier = makeSpecifier(*str <= 'Z' ? *str + 32 : *str, sized); // lowercase is used for specifiers anyway
        return specifier;
    }


    /** Extract all information about the next specifier */
    constexpr SpecifierType readSpecifier(char * specifierStr, const char *& str)
    {
        SpecifierType specifier;
        bool sized = false;
        char c = *str;
        while (c && isIn(c, ignoreTable))
        {
            if (c == '*') sized = true;
            *specifierStr++ = c;
            c = *++str;
        }
        if (!c) return specifier; // Shouldn't be short
        if ((specifier.first = getSubtype(str)) != SubType::None) {
            *specifierStr++ = *str++;
            if ((specifier.second = getSubtype(str)) != SubType::None) *specifierStr++ = *str++;
        }
        if (!isInCaseless(*str, specifiers)) return specifier;
        specifier.specifier = makeSpecifier(*str <= 'Z' ? *str + 32 : *str, sized); // lowercase is used for specifiers anyway
        *specifierStr++ = *str;
        *specifierStr = 0;
        return specifier;
    }
    /** Count the number of arguments.
        This takes care of %*d and %.*s dual specifiers */
    constexpr std::size_t countArguments(const char * str) {
        std::size_t count = 0, sized = 0;
        while (SpecifierType specifier = readIthSpecifier(count, str))
        {
            if (specifier.hasLength()) sized++;
            count++;
        }
        return count + sized;
    }
    /** Count the number of arguments.
        This takes care of %*d and %.*s dual specifiers */
    constexpr std::size_t countSpecifiers(const char * str) {
        std::size_t count = 0;
        while ((str = nextIthSpecifier(0, str)))
            count++;
        return count;
    }

    /** This is probably the most cryptic part of the parsing.
        This builds an table of arguments saving and loading (from the log buffer) function pointers, from a compile time string.
        @param N should be the number of valid specifiers in the formatting string */
    template <std::size_t N>
    struct SpecifiersTable
    {
        static constexpr std::size_t size = N;

        /** The constructor needs a value template (in S, requiring a str_ref instance). The argument for the constructor is ignored and only used to deduce the template argument */
        template <typename S>
        consteval SpecifiersTable(const S)
        {
            constexpr std::size_t argCount = countSpecifiers((const char*)S::data);
            static_assert(argCount == N, "The number of specifiers in the format string doesn't match the number of arguments passed in");
        }

        /** Get the expected argument type for the i-th specifier */
        template <typename S, std::size_t i>
        static constexpr auto getArgument()
        {
            constexpr SpecifierType specifier = readIthSpecifier(i, S::data);
            static_assert(specifier, "Invalid specifier in the format string");
            using Type = decltype(getExpectedType<specifier>());
            static_assert(!std::is_same_v<Type, InvalidSpecifiedType>, "The specifier doesn't map to a valid type ");
            return Type {};
        }

        /** Gets a typelist for all the specifiers (used to compare that they match with the actual arguments) */
        template <typename S, size_t... I>
        static constexpr auto getLooselyTypedArguments(const S, std::index_sequence<I...>)
        {
            return typename DecayedPromotedTypeList<typename Flatten<TypeList<decltype(getArgument<S, I>())...>>::Type>::Type{};
        }

        /** Gets converted arguments type. This accounts for char * / void * expectation and default promotion rules in order to convert the given arguments to the expected type */
        template <typename S, size_t... I>
        static constexpr auto getPromotedArgumentsType(const S, std::index_sequence<I...>)
        {
            return typename DecayedSmartPromotedTypeList<decltype(getArgument<S, I>())...>::Type{};
        }

    };

    /** Make sure only the file name is saved and not the whole file path */
    struct sourceloc
    {
        constexpr sourceloc(const char * filename, const int line) : _filename(filename), _line(line){}
        const char * _filename;
        const int _line;
        inline const char * file_name() const { return _filename; }
        inline int line() const { return _line; }
    };
    #define SourceLoc CompileTime::sourceloc{CompileTime::str_ref<CompileTime::basename<CompileTime::str{__FILE__}>()>{}.data, __LINE__}
//    using sourceloc = std::source_location;

    /** Storing arguments to the log buffer is a bit more complex because the expanded argument array can't be iterated
        with the parsed (specifier) type list, they don't have the same size. A specifier "%.*s" maps to a TypeList<int, char*>
        (so a single type) while it'll consume 2 arguments (int for size and const char * for the actual string).
        We also don't want to flatten the specifier type list, else we'll store the int and the char * as a integral and a pointer (which might not exists anymore when dumped)
        while we must store as a length limited string instead.

        So it's not possible to write a fold expression that could match a type conversion for each argument, since they aren't 1:1

        However, we can store all arguments to a tuple, and then expand the specifier typelist, keeping track of the offset in the tuple.
        The offset can be incremented by 1 for standard arguments, but by 2 for sized specifiers
     */
    template <typename T>
    struct StoreArgumentInLogBuffer
    {
        template <typename A>
        static constexpr bool store(const A & a) { T t = (T)std::get<0>(a); return Log::logBuffer.save(t); }
#if UseLogCompression == 1
        template <typename A>
        static constexpr bool check(uint32 & readPos, const A & a) { T t = (T)std::get<0>(a); return Log::logBuffer.matchValue(readPos, t); }
#endif
        static constexpr std::size_t consumed = 1;
    };
    template <>
    struct StoreArgumentInLogBuffer<char*>
    {
        template <typename A>
        static constexpr bool store(const A & a) { char * t = (char*)std::get<0>(a); return Log::logBuffer.saveString(t, 0); }
#if UseLogCompression == 1
        template <typename A>
        static constexpr bool check(uint32 & readPos, const A & a) { char * t = (char*)std::get<0>(a); return Log::logBuffer.matchValue(readPos, t, strlen(t)); }
#endif
        static constexpr std::size_t consumed = 1;
    };
    template <>
    struct StoreArgumentInLogBuffer<TypeList<>>
    {
        template <typename A>
        static constexpr bool store(const A & a) { return true; }
#if UseLogCompression == 1
        template <typename A>
        static constexpr bool check(uint32 & readPos, const A & a) { return true; }
#endif

    };
    template <typename T>
    struct StoreArgumentInLogBuffer<TypeList<int, T>>
    {
        template <typename A>
        static constexpr bool store(const A & a) { int len = std::get<0>(a); T type = (T)std::get<1>(a); return Log::logBuffer.save(len) && Log::logBuffer.save(type); }
#if UseLogCompression == 1
        template <typename A>
        static constexpr bool check(uint32 & readPos, const A & a) { int len = std::get<0>(a); T type = (T)std::get<1>(a); return Log::logBuffer.matchValue(readPos, len) && Log::logBuffer.matchValue(readPos, type); }
#endif
        static constexpr std::size_t consumed = 2;
    };
    template <>
    struct StoreArgumentInLogBuffer<TypeList<int, char*>>
    {
        template <typename A>
        static constexpr bool store(const A & a) { int len = std::get<0>(a); char * type = (char*)std::get<1>(a); return Log::logBuffer.saveString(type, len); }
#if UseLogCompression == 1
        template <typename A>
        static constexpr bool check(uint32 & readPos, const A & a) { int len = std::get<0>(a); char * type = (char*)std::get<1>(a); return Log::logBuffer.matchValue(readPos, type, len); }
#endif
        static constexpr std::size_t consumed = 2;
    };


    template<typename Tuple>
    constexpr auto popTupleFront(const Tuple & tuple) {
        static_assert(std::tuple_size<Tuple>::value > 0, "Cannot pop from an empty tuple");
        return std::apply(
            [](auto, auto... rest) { return std::make_tuple(rest...); },
            tuple);
    }


    template <typename Type>
    struct StoreArgumentsInBuffer
    {
        template <typename Tuple>
        static constexpr auto store(bool & result, const Tuple & a)
        {
            result = result && StoreArgumentInLogBuffer<Type>::store(a);
            if constexpr (StoreArgumentInLogBuffer<Type>::consumed == 2)
                return popTupleFront(popTupleFront(a));
            else return popTupleFront(a);
        }
#if UseLogCompression == 1
        template <typename Tuple>
        static constexpr auto check(bool & result, uint32 & readPos, const Tuple & a)
        {
            result = result && StoreArgumentInLogBuffer<Type>::check(readPos, a);
            if constexpr (StoreArgumentInLogBuffer<Type>::consumed == 2)
                return popTupleFront(popTupleFront(a));
            else return popTupleFront(a);
        }
#endif
    };

    template <>
    struct StoreArgumentsInBuffer<const TypeList<>>
    {
        template <typename Tuple>
        static constexpr auto store(bool & result, const Tuple & a) { return a; }
#if UseLogCompression == 1
        template <typename Tuple>
        static constexpr auto check(bool & result, uint32 & readPos, const Tuple & a) { return a; }
#endif

    };
    template <typename Type>
    struct StoreArgumentsInBuffer<const TypeList<Type>>
    {
        template <typename Tuple>
        static constexpr auto store(bool & result, const Tuple & a) { return StoreArgumentsInBuffer<Type>::store(result, a); }
#if UseLogCompression == 1
        template <typename Tuple>
        static constexpr auto check(bool & result, uint32 & readPos, const Tuple & a) { return StoreArgumentsInBuffer<Type>::check(result, readPos, a); }
#endif
    };

    template <typename Type, typename ... Types>
    struct StoreArgumentsInBuffer<const TypeList<Type, Types...>>
    {
        template <typename Tuple>
        static constexpr auto store(bool & result, const Tuple & a)
        {
            return StoreArgumentsInBuffer<const TypeList<Types...>>::store(result, StoreArgumentsInBuffer<Type>::store(result, a));
        }

#if UseLogCompression == 1
        template <typename Tuple>
        static constexpr auto check(bool & result, uint32 & readPos, const Tuple & a)
        {
            return StoreArgumentsInBuffer<const TypeList<Types...>>::check(result, readPos, StoreArgumentsInBuffer<Type>::check(result, readPos, a));
        }
#endif
    };

    template <typename TL, typename... Args>
    constexpr static bool storeArgumentsInLogBuffer(Args && ... args)
    {
        auto tup = std::make_tuple(args...);
        bool result = true;
        StoreArgumentsInBuffer<TL>::store(result, tup);
        return result;
    }

#if UseLogCompression == 1
    template <typename TL, typename... Args>
    constexpr static bool checkForSameRepeatedArgumentsInLogBuffer(uint32 r, Args && ... args)
    {
        auto tup = std::make_tuple(args...);
        bool result = true;
        // r is set to the position in the buffer for the first parameter to check against
        StoreArgumentsInBuffer<TL>::check(result, r, tup);
        return result;
    }
#endif

    struct LogItemSaver : public Log::LogItem
    {
        LogItemSaver(const char * str, const uint32 mask, const bool saveLine, const sourceloc * loc) : lock(Log::logBuffer.mutex)
        {
            if ((mask & 3) == 3 && (mask & Log::logMask) == 0) return;
            kept = true;
            FileDump = loc ? 1 : 0;
            LineDump = saveLine ? 1 : 0;
            MaskType = mask < 4 ? mask : 3;
            saveFormat(str);

#if defined(StoreLogSizeType) || UseLogCompression == 1
            wp = Log::logBuffer.fetchWritePos(); // Save the current write position just after the item.
            // Image of the log buffer for a log item is expected like this:
            // [     R  ... W            ]
            // [-------------------------]
            // [            LSFLMPPPPC   ] with L: log item, S: store log size type, F: opt. file name pointer address, L: opt. line, M: opt. Mask, C: opt. count, P: opt. parameters

#endif
#if UseLogCompression == 1
            // Compare the last log format to check if it's a repetition
            if (Log::logBuffer.lastLogPos <= Log::logBuffer.sm1)
            {
                Log::LogItem previous{};
                if (Log::logBuffer.loadTypeAt(Log::logBuffer.lastLogPos, previous) && previous.Param == 0 && previous.isRepeated(*this))
                {
                    // Ok, the log appears to be a repetition, so let's mark it as so
                    repeated = true;

                    uint8 buf[9] = {};
                    uint8 locSize = loc ? Log::encode_u64(buf, Log::LogItem::computeAddress(loc->file_name())) : 0;
                    uint8 lineSize = loc && saveLine ? Log::encode_u64(buf, loc->line()) : 0;
                    uint8 maskSize = mask > 3 ? Log::encode_u64(buf, mask) : 0;
                    firstParamPos = Log::logBuffer.lastLogPos + sizeof(StoreLogSizeType) + sizeof(previous) + locSize + lineSize + maskSize;
                    return; // Skip saving anything here to the log buffer
                }
            }
#endif
            Log::logBuffer.saveType(*(const Log::LogItem *)this); // Save the log item first
#ifdef StoreLogSizeType
            StoreLogSizeType blank {};
            Log::logBuffer.saveType(blank); // This reserves the space for the storing the number of bytes used for this item in the buffer
#endif
            if (loc) Log::logBuffer.save(Log::LogItem::computeAddress(loc->file_name())); // Don't save the string here since it should be in the binary, so only store its pointer
            if (loc && saveLine) Log::logBuffer.save((uint64)loc->line());
            if (mask > 3) Log::logBuffer.save(mask);
        }
#if defined(StoreLogSizeType) || UseLogCompression == 1
        uint32 wp;
#endif
        Log::ScopedLock lock;
        bool kept = false;
#if UseLogCompression == 1
        bool repeated = false;
        uint32 firstParamPos = 0;
#endif
    };

    static inline void reportError(const char * string)
    {
#if ThrowOnError == 1
        throw Log::Exception(string);
#else
        Log::errorStoringArgumentsFor(string);
#endif
    }

    template <const auto string>
    struct LogFormatter : public LogItemSaver
    {
        static constexpr std::size_t size = string.size;
        static constexpr std::size_t N = countSpecifiers(string);
        static constexpr auto impl_string = str_ref<string>{};
        static constexpr SpecifiersTable<N> arguments = {impl_string};


        template <typename... Args>
        void storeArguments(Args && ... args)
        {
            constexpr auto argList = typename DecayedPromotedTypeList<Args...>::Type{};
            constexpr std::size_t expArgCount = countArguments(string);
            static_assert(expArgCount == sizeof...(Args), "The number of arguments in the format specifier doesn't match the number of arguments passed in");
            static_assert(std::is_same_v<std::decay_t<decltype(argList)>, decltype(SpecifiersTable<N>::getLooselyTypedArguments(impl_string, std::make_index_sequence<N>{}))>, "The expected arguments doesn't match those in the format specifier");
            if (!kept) return; // Log isn't saved, skip this
            // Here we've a problem. The given argument list promotion is different depending on the specifier.
            // Typically, if given a %p, we store a pointer (and we don't care if the pointed object still exist when the log will be dumped later on), so any pointer should be converted to void*.
            // But if given a %s, we have to store a string (don't convert to void*), since the pointed string might not exists anymore once we'll dump the log.
            // Also, if given a %.*s, the string might not be zero terminated so when storing it, we have to only store (and access) the expected characters length.
            constexpr auto type = SpecifiersTable<N>::getPromotedArgumentsType(impl_string, std::make_index_sequence<N>{});
#if UseLogCompression == 1
            Log::LogItem previous{};
            if (repeated && checkForSameRepeatedArgumentsInLogBuffer<decltype(type)>(firstParamPos, std::forward<Args>(args)...))
            {   // Need to modify the count in the previous log item
                Log::logBuffer.loadTypeAt(Log::logBuffer.lastLogPos, previous);
                previous.Repeat = 1;
                Log::logBuffer.saveTypeAt(Log::logBuffer.lastLogPos, previous);

                // Fix the buffer now
                // [     R  ... W            ]
                // [-------------------------]
                // [            LSFLMPPPPC   ] with L: log item, S: store log size type, F: opt. file name pointer address, L: opt. line, M: opt. Mask, C: opt. count, P: opt. parameters

                StoreLogSizeType size = 0, count = 0;
                if (   !Log::logBuffer.loadTypeAt(Log::logBuffer.lastLogPos + sizeof(previous), size)
                    || !size || !Log::logBuffer.loadTypeAt(Log::logBuffer.lastLogPos + sizeof(previous) + size + sizeof(size), count))
                {
                    // It's possible the count doesn't exist yet, so we need to save it first
                    StoreLogSizeType one = (StoreLogSizeType)0;
                    if (!Log::logBuffer.saveType(one))
                        reportError("Can't save repeat count");
                    return;
                }
                count++;
                if (!Log::logBuffer.saveTypeAt(Log::logBuffer.lastLogPos + sizeof(previous) + size + sizeof(size), count))
                {
                    reportError("Can't save repeat count");
                }
                return;
            }

            if (repeated)
            {
                Log::logBuffer.loadTypeAt(Log::logBuffer.lastLogPos, previous);
                previous.Repeat = 1;
                previous.Param = 1;
                Log::logBuffer.saveTypeAt(Log::logBuffer.lastLogPos, previous);

                // We now have to save the additional parameters after the previous storage, that is like this:
                // [     R  ...          W   ]
                // [-------------------------]
                // [P           LSFLMPPPPCPPP] with L: log item, S: store log size type, F: opt. file name pointer address, L: opt. line, M: opt. Mask, C: opt. size of parameters, P: opt. parameters
                StoreLogSizeType blank {};
                Log::logBuffer.saveType(blank); // This reserves the space for the storing the number of bytes used for the parameters in the buffer
            }
#endif

            if (!storeArgumentsInLogBuffer<decltype(type)>(std::forward<Args>(args)...))
            {   // ISSUE HERE, what to do if we can't store the log? Drop it? Silently?
                reportError(string);
            }

#if UseLogCompression == 1
            if (repeated)
            {   // We need to say that the arguments are different, so we need to mark them here (hopefully, it's the same as a normal storage, except for the log item itself)
                uint32 cp = Log::logBuffer.fetchWritePos();
                // Ugly modulo calculus without a division, computing (cp - wp - sizeof(StoreLogSizeType), that is, the distance between last P and C in the previous diagram
                uint32 s = (cp + (Log::logBuffer.sm1 + 1) - wp - sizeof(StoreLogSizeType)) & Log::logBuffer.sm1;
                if (s >= 1<<(sizeof(StoreLogSizeType)*8))
                {   // Another ISSUE HERE, we can't store the actual consumed size in the given type
                    // So we decide to revert back the log item to before the saving, this log is lost anyway, we'll never be able to remove it afterward. Let's leave a trace here
                    Log::logBuffer.rollbackWrite(wp);
                    // We can't revert the Param to 0, since we might be the 3rd repeated log with params.
                    reportError(string);
                }
                Log::logBuffer.saveTypeAt(wp, (StoreLogSizeType)s);
                return;
            }
#endif

#ifdef StoreLogSizeType
            uint32 cp = Log::logBuffer.fetchWritePos();
            // Ugly modulo calculus without a division, computing (cp - wp - sizeof(LogItem) - sizeof(StoreLogSizeType), that is, the distance between C and F in the previous diagram
            uint32 s = (cp + (Log::logBuffer.sm1 + 1) - wp - sizeof(Log::LogItem) - sizeof(StoreLogSizeType)) & Log::logBuffer.sm1;
            if (s >= 1<<(sizeof(StoreLogSizeType)*8))
            {   // Another ISSUE HERE, we can't store the actual consumed size in the given type
                // So we decide to revert back the log item to before the saving, this log is lost anyway, we'll never be able to remove it afterward. Let's leave a trace here
                Log::logBuffer.rollbackWrite(wp);
                reportError(string);
            }
            Log::logBuffer.rollbackWrite((wp + sizeof(Log::LogItem)) & Log::logBuffer.sm1);
            Log::logBuffer.saveType((StoreLogSizeType)s);
            Log::logBuffer.rollbackWrite(cp);
#endif
#if UseLogCompression == 1
            if (!repeated)
                Log::logBuffer.lastLogPos = wp;
#endif
        }

#ifdef UseLogCallback
        void invokeCallback(const sourceloc * loc, const uint32 mask, ...)
        {
            if (kept && Log::LogCallback)
            {
                va_list vaargs;
                va_start(vaargs, mask);
                Log::LogCallback(loc ? loc->file_name() : "", loc ? loc->line() : 0, mask, string, vaargs);
                va_end(vaargs);
            }
        }
#else
        template <typename... Args>
        void invokeCallback(const sourceloc * loc, const uint32 mask, const Args & ... args) {}
#endif
        template <typename... Args>
        LogFormatter(const sourceloc & loc, Args... args) : LogItemSaver((const char*)string, Log::LogMask::Default, false, &loc) { invokeCallback(&loc, Log::LogMask::Default, args...); storeArguments(std::forward<Args>(args)...); }

        template <typename... Args>
        LogFormatter(const bool, const sourceloc & loc, Args... args) : LogItemSaver((const char*)string, Log::LogMask::Default, true, &loc) { invokeCallback(&loc, Log::LogMask::Default, args...); storeArguments(std::forward<Args>(args)...); }

        template <typename... Args>
        LogFormatter(const uint32 mask, const bool, const sourceloc & loc,  Args... args) : LogItemSaver((const char*)string, mask, true, &loc) { invokeCallback(&loc, mask, args...); storeArguments(std::forward<Args>(args)...); }

        template <typename... Args>
        LogFormatter(const uint32 mask, Args... args) : LogItemSaver((const char*)string, mask, false, nullptr) { invokeCallback(nullptr, mask, args...); storeArguments(std::forward<Args>(args)...); }

        template <typename... Args>
        static constexpr auto getActualTypeList(Args... args)
        {
            return TypeList<Args...>{};
        }
    };

    template <typename T>
    bool computeSizeForType(std::size_t & required, const char * spec, const std::size_t * size)
    {
        T val = {};
        if (!Log::logBuffer.load(val)) return false;
        required += size ? snprintf(nullptr, 0, spec, *size, val) : snprintf(nullptr, 0, spec, val);
        return true;
    }

    /** A string class that's using a buffer allocated on the stack */
    struct StackString
    {
        const char * buffer;
        char * ptr;
        std::size_t allocSize;

        StackString(char * ptr, const std::size_t allocSize) : buffer(ptr), ptr(ptr), allocSize(allocSize) {}

        StackString & operator += (const char * str) { return this->append(str); }
        StackString & append(const char * str)
        {
            if (!ptr) allocSize += strlen(str);
            else { strcpy(ptr, str); ptr += strlen(str); } return *this;
        }
        StackString & append(const char * str, const std::size_t len)
        {
            if (!ptr) allocSize += len;
            else { memcpy(ptr, str, len); ptr += len; } return *this;
        }
        // This is to avoid having numerous instance of this function for all types, since it'll default to
        StackString & appendV(const char * format, ...)
        {
            va_list argp;
            va_start(argp, format);
            if (!ptr) allocSize += vsnprintf(nullptr, 0, format, argp);
            else {
                std::size_t len = vsnprintf(ptr, allocSize - (ptr - buffer), format, argp);
                ptr += len;
            }
            va_end(argp);
            return *this;
        }
    };
    template <typename T>
    bool logSpecifier(StackString & str, const char * spec, const std::size_t * size)
    {
        T val = {};
        if (!Log::logBuffer.load(val)) return false;
        if (size) str.appendV(spec, *size, val); else str.appendV(spec, val);
        return true;
    }

    bool dumpLogImpl(const std::size_t argCount, const char * file, const std::size_t line,  const char * format, StackString & s);

    template <typename T>
    bool dumpLog(const T & func)
    {
        // Fetch the log item first
        const uint32 readPos = Log::logBuffer.fetchReadPos();
        Log::LogItem item;
        if (!Log::logBuffer.loadType(item)) return false;
#ifdef StoreLogSizeType
        StoreLogSizeType blank;
        if (!Log::logBuffer.loadType(blank)) return false;
#endif

        // Check if we need to format file and line and mask first
        std::uintptr_t filePtr = 0;
        std::size_t line = 0;
        uint32 mask = 0;
        if (item.FileDump && !Log::logBuffer.load(filePtr)) return false;
        if (item.LineDump && !Log::logBuffer.load(line)) return false;
        if (item.MaskType == 3 && !Log::logBuffer.load(mask)) return false;
        const char * file = filePtr ? Log::LogItem::makePointer(filePtr) : nullptr;

        // Extract all arguments now
        const uint32 argPos = Log::logBuffer.fetchReadPos();
        const char * format = item.loadFormat();
        const std::size_t specCount = countSpecifiers(format);
        StackString sizeCounter{nullptr,0};
        // Dump the log item to a invalid buffer to count the required allocation size
        if (!dumpLogImpl(specCount, file, line, format, sizeCounter)) return false;

        // Now we have the length, let's allocate and create the final string here
        Log::logBuffer.rollback(argPos);
        char * buffer = (char*)alloca(sizeCounter.allocSize + 1);
        StackString s(buffer, sizeCounter.allocSize + 1);

        if (!dumpLogImpl(specCount, file, line, format, s)) return false;

#if UseLogCompression == 1
        int count = 0;
        if (item.Repeat && !item.Param)
        {
            StoreLogSizeType c = 0;
            if (!Log::logBuffer.load(c)) return false;
            count = c+1;
        }
        // And call the callback with that string
        if constexpr(std::is_same_v<decltype(func(s.buffer, mask, count)), bool>)
        {
            bool ret = func(s.buffer, mask, count);
            if (!ret) { Log::logBuffer.rollback(readPos); return false; }
        } else func(s.buffer, mask, count);

        if (item.Param)
        {   // Now we have the repeated log, let's dump it from here directly
            if (!Log::logBuffer.load(count)) return false;
            const uint32 argPos2 = Log::logBuffer.fetchReadPos();
            StackString sizeCounter2{nullptr,0};
            // Dump the log item to a invalid buffer to count the required allocation size
            if (!dumpLogImpl(specCount, file, line, format, sizeCounter2)) return false;

            Log::logBuffer.rollback(argPos2);
            char * buffer2 = (char*)alloca(sizeCounter.allocSize + 1);
            StackString s2(buffer, sizeCounter.allocSize + 1);

            if (!dumpLogImpl(specCount, file, line, format, s2)) return false;

            if constexpr(std::is_same_v<decltype(func(s2.buffer, mask, -1)), bool>)
            {
                bool ret = func(s2.buffer, mask, -1);
                if (!ret) { Log::logBuffer.rollback(readPos); return false; }
            } else func(s2.buffer, mask, -1);
        }
#else
        // And call the callback with that string
        if constexpr(std::is_same_v<decltype(func(s.buffer, mask)), bool>)
        {
            bool ret = func(s.buffer, mask);
            if (!ret) Log::logBuffer.rollback(readPos);
            return ret;
        } else func(s.buffer, mask);
#endif
        return true;
    }
}

// We are using a syntactic sugar macro here to make the code more usual for any user
#define elog(fmt, ...)               CompileTime::LogFormatter<CompileTime::str{fmt}>{Log::LogMask::Default, __VA_ARGS__}
#define elogf(fmt, ...)              CompileTime::LogFormatter<CompileTime::str{fmt}>{SourceLoc, __VA_ARGS__}
#define elogfl(fmt, ...)             CompileTime::LogFormatter<CompileTime::str{fmt}>{true, SourceLoc, __VA_ARGS__}
#define elogm(mask, fmt, ...)        CompileTime::LogFormatter<CompileTime::str{fmt}>{mask, __VA_ARGS__}
#define elogflm(mask, fmt, ...)      CompileTime::LogFormatter<CompileTime::str{fmt}>{mask, true, SourceLoc, __VA_ARGS__}
#define elogt(mask, fmt, loc, ...)   CompileTime::LogFormatter<CompileTime::str{fmt}>{true, loc, __VA_ARGS__}
