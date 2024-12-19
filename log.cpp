// We need our declarations
#include "log.hpp"

namespace Log
{
#ifdef UseLogCallback
    void (*LogCallback)(const char * file, const int line, const uint32 mask, const char * format, va_list args) = nullptr;
#endif

    uint8 encodedLen(uint8 b)
    {
        if (b < 0b10000000) return 1;
        if (b < 0b11000000) return 2;
        if (b < 0b11100000) return 3;
        if (b < 0b11110000) return 4;
        return ((b & 0x0F) + 2);
    }
    uint8 encode_u64(uint8 buf[9], uint64 x)
    {
        if (x < 0x80)
        {
            buf[0] = (uint8)x;
            return 1;
        }
        if (x < 0x10000000) {
            if (x < 0x00004000) {
                x <<= 2;
                buf[0] = 0x80 | (((uint8)x) >> 2);
                buf[1] = (uint8)(x >> 8);
                return 2;
            }
            if (x < 0x00200000) {
                x <<= 3;
                buf[0] = 0xC0 | (((uint8)x) >> 3);
                buf[1] = (uint8)(x >> 8);
                buf[2] = (uint8)(x >> 16);
                return 3;
            }
            x <<= 4;
            buf[0] = 0xE0 | (((uint8)x) >> 4);
            buf[1] = (uint8)(x >> 8);
            buf[2] = (uint8)(x >> 16);
            buf[3] = (uint8)(x >> 24);
            return 4;
        }

        memcpy(buf+1, &x, sizeof(x));

        const uint8 LEN_MASK = 0b111;
        uint8 len = ((uint8)(std::countl_zero(x) >> 3)) ^ LEN_MASK;
        buf[0] = 0xF0 | len;
        return (len + 2);
    }
    uint8 decode_u64(const uint8 buf[9], uint64 & x)
    {
        uint8 buf0 = buf[0];
        if ((buf0 & 0x80) == 0) {
            x = buf0;
            return 1;
        }
        if (buf0 < 0xF0)
        {
            if ((buf0 & 0b01000000) == 0) {
                uint32 low = buf0 & 0b00111111;
                x = (((uint32)buf[1]) << 6) | low;
                return 2;
            }
            if ((buf0 & 0b00100000) == 0) {
                uint32 low = buf0 & 0b00011111;
                x = ((uint32)buf[2] << 13) | ((uint32)buf[1] << 5) | low;
                return 3;
            }
            x =  ((uint32)buf[3] << 20)
                | ((uint32)buf[2] << 12)
                | ((uint32)buf[1] << 4)
                | ((uint32)buf0 & 0b00001111);
            return 4;
        }

        const uint8 LEN_MASK = 0b111;
        uint8 len = buf[0] & 0x0F;
        uint8 mask_octets = (len & LEN_MASK) ^ LEN_MASK;
        uint64 mask = uint64(-1) >> (mask_octets * 8);
        memcpy(&x, buf+1, sizeof(x));
        x &= mask;
        return (len + 2);
    }
    uint8 encode_i64(uint8 buf[9], int64 x)
    {
        const uint8 zigzagShift = sizeof(x) * 8 - 1;
        uint64 zigzag = (uint64)(x >> zigzagShift) ^ (uint64)(x << 1);
        return encode_u64(buf, zigzag);
    }
    uint8 decode_i64(const uint8 buf[9], int64 & x)
    {
        uint64 zigzag = 0;
        uint8 ret = decode_u64(buf, zigzag);
        x = (int64)(zigzag >> 1) ^ (-int64(zigzag & 1));
        return ret;
    }
    uint8 encode_f64(uint8 buf[9], double d)
    {
        uint64 x; memcpy(&x, &d, sizeof(x));
        x = byteswap(x);
        return encode_u64(buf, x);
    }
    uint8 decode_f64(uint8 buf[9], double & d)
    {
        uint64 x;
        uint8 ret = decode_u64(buf, x);
        x = byteswap(x);
        memcpy(&d, &x, sizeof(x));
        return ret;
    }
}

namespace CompileTime
{
#if DeleteOldLogsWhenFull == 1
  #ifndef StoreLogSizeType
    static bool extractFirstLog(const Log::LogItem & item)
    {
        std::uintptr_t ptr = 0; std::size_t siz = 0; uint32 msk = 0;
        if (item.FileDump && !Log::logBuffer.skipLoad<std::uintptr_t>()) return false;
        if (item.LineDump && !Log::logBuffer.skipLoad<std::size_t>()) return false;
        if (item.MaskType == 3 && !Log::logBuffer.skipLoad<uint32>()) return false;

        const char * format = item.loadFormat();

        const std::size_t argCount = countSpecifiers(format);
        const char * str = format;
        char specifierString[sizeof("%0.18446744073709551615lls")] = {'%'}; // That's the maximum size the specifier can use in printf (but that would be very crazy to do so)
        for (auto i = 0; i < argCount; i++)
        {
            const char * spec = nextSpecifier(str);
            // Store the textual size
            SpecifierType stype = readSpecifier(&specifierString[1], spec);
            if (!stype) continue; // Invalid specifier is ignored and output as is
            bool sized = stype.hasLength();
            // Merge subtype now, so that 'hh' becomes 'H' and 'll' becomes 'b'
            char type = (char)stype.first;
            if (stype.second != SubType::None) type = (stype.first == SubType::Short) ? (char)stype.second  - 32 : (stype.first == SubType::Long) ? (char)stype.second - 32 : (char)stype.second;

            std::size_t _size = 0;
            Specifier finalSpec = simplify(specifierType(stype.specifier));
            if (sized)
            {   // Extract the argument for the size
                if (finalSpec != Specifier::String && !Log::logBuffer.load(_size)) return false;
            }
            std::size_t * size = sized ? &_size : nullptr;

            switch (finalSpec)
            {
            case Specifier::Double:
            {
                if (type == 'L') { if (!Log::logBuffer.skipLoad<long double>()) return false; }
                else if (!Log::logBuffer.skipLoad<double>()) return false;
            } break;
            case Specifier::SignedDecInt:
            {
                switch(type)
                {
                case 'h': if (!Log::logBuffer.skipLoad<short int>()) return false; break;
                case 'H': if (!Log::logBuffer.skipLoad<signed char>()) return false; break;
                case 'l': if (!Log::logBuffer.skipLoad<long int>()) return false; break;
                case 'b': if (!Log::logBuffer.skipLoad<long long int>()) return false; break;
                case 'j': if (!Log::logBuffer.skipLoad<intmax_t>()) return false; break;
                case 'z': if (!Log::logBuffer.skipLoad<std::size_t>()) return false; break;
                case 't': if (!Log::logBuffer.skipLoad<ptrdiff_t>()) return false; break;
                case '\0': if (!Log::logBuffer.skipLoad<int>()) return false; break;
                default: break; // Invalid specifier is ignored and output as is
                }
            } break;
            case Specifier::UnsignedDecInt:
            {
                switch(type)
                {
                case 'h': if (!Log::logBuffer.skipLoad<unsigned short int>()) return false; break;
                case 'H': if (!Log::logBuffer.skipLoad<unsigned char>()) return false; break;
                case 'l': if (!Log::logBuffer.skipLoad<unsigned long int>()) return false; break;
                case 'b': if (!Log::logBuffer.skipLoad<unsigned long long int>()) return false; break;
                case 'j': if (!Log::logBuffer.skipLoad<uintmax_t>()) return false; break;
                case 'z': if (!Log::logBuffer.skipLoad<std::size_t>()) return false; break;
                case 't': if (!Log::logBuffer.skipLoad<ptrdiff_t>()) return false; break;
                case '\0': if (!Log::logBuffer.skipLoad<unsigned>()) return false; break;
                default: break; // Invalid specifier is ignored and output as is
                }
            } break;
            case Specifier::String:
            {
                //if (!sized && !Log::logBuffer.load(_size)) return false;
                std::size_t len = 0;
                if (!Log::logBuffer.loadString(nullptr, len)) return false;
                if (!Log::logBuffer.consume(len)) return false;
            } break;
            case Specifier::Char:
            {
                if (!Log::logBuffer.skipLoad<int>()) return false;
            } break;
            case Specifier::Pointer:
            {
                if (!Log::logBuffer.skipLoad<const void*>()) return false;
            }
            case Specifier::Unknown: break; // Invalid specifier is ignored and output as is
            }
            str = spec+1;
        }
        // Finally append what remains here
        return true;
    }
  #endif
    bool extractFirstLog()
    {
#ifndef StoreLogSizeType
        const uint32 r = Log::logBuffer.fetchReadPos();
#endif
        Log::LogItem item;
        if (!Log::logBuffer.loadType(item)) return false;
#ifdef StoreLogSizeType
        StoreLogSizeType size;
        if (!Log::logBuffer.loadType(size)) return false;
        return Log::logBuffer.consume(size); // Already consumed the size value above, that's not included in the computation
#else
        // Check if we need to format file and line and mask first
        if (!extractFirstLog(item))
        {
            Log::logBuffer.rollback(r);
            return false;
        }
        return true;
#endif
    }
#endif


    bool dumpLogImpl(const std::size_t argCount, const char * file, const std::size_t line,  const char * format, StackString & s)
    {
        if (file) s.append(file);
        if (line) s.appendV(":%u", line);
        if (file || line) s.append(" ");

        const char * str = format;
        char specifierString[sizeof("%0.18446744073709551615lls")] = {'%'}; // That's the maximum size the specifier can use in printf (but that would be very crazy to do so)
        for (auto i = 0; i < argCount; i++)
        {
            const char * spec = nextSpecifier(str, true);
            if (*spec == '%') { s.append(str, spec - str); str = spec + 1; i--; continue; } // Escaped character is simply copied over
            // Store the textual size
            s.append(str, spec - str - 1);

            SpecifierType stype = readSpecifier(&specifierString[1], spec);
            if (!stype) continue; // Invalid specifier is ignored and output as is
            bool sized = stype.hasLength();
            // Merge subtype now, so that 'hh' becomes 'H' and 'll' becomes 'b'
            char type = (char)stype.first;
            if (stype.second != SubType::None) type = (stype.first == SubType::Short) ? (char)stype.second  - 32 : (stype.first == SubType::Long) ? (char)stype.second - 32 : (char)stype.second;

            std::size_t _size = 0;
            Specifier finalSpec = simplify(specifierType(stype.specifier));
            if (sized)
            {   // Extract the argument for the size
                if (finalSpec != Specifier::String && !Log::logBuffer.load(_size)) return false;
            }
            std::size_t * size = sized ? &_size : nullptr;

            switch (finalSpec)
            {
            case Specifier::Double:
            {
                if (type == 'L') { if (!logSpecifier<long double>(s, specifierString, size)) return false; }
                else if (!logSpecifier<double>(s, specifierString, size)) return false;
            } break;
            case Specifier::SignedDecInt:
            {
                switch(type)
                {
                case 'h': if (!logSpecifier<short int>(s, specifierString, size)) return false; break;
                case 'H': if (!logSpecifier<signed char>(s, specifierString, size)) return false; break;
                case 'l': if (!logSpecifier<long int>(s, specifierString, size)) return false; break;
                case 'b': if (!logSpecifier<long long int>(s, specifierString, size)) return false; break;
                case 'j': if (!logSpecifier<intmax_t>(s, specifierString, size)) return false; break;
                case 'z': if (!logSpecifier<std::size_t>(s, specifierString, size)) return false; break;
                case 't': if (!logSpecifier<ptrdiff_t>(s, specifierString, size)) return false; break;
                case '\0': if (!logSpecifier<int>(s, specifierString, size)) return false; break;
                default: break; // Invalid specifier is ignored and output as is
                }
            } break;
            case Specifier::UnsignedDecInt:
            {
                switch(type)
                {
                case 'h': if (!logSpecifier<unsigned short int>(s, specifierString, size)) return false; break;
                case 'H': if (!logSpecifier<unsigned char>(s, specifierString, size)) return false; break;
                case 'l': if (!logSpecifier<unsigned long int>(s, specifierString, size)) return false; break;
                case 'b': if (!logSpecifier<unsigned long long int>(s, specifierString, size)) return false; break;
                case 'j': if (!logSpecifier<uintmax_t>(s, specifierString, size)) return false; break;
                case 'z': if (!logSpecifier<std::size_t>(s, specifierString, size)) return false; break;
                case 't': if (!logSpecifier<ptrdiff_t>(s, specifierString, size)) return false; break;
                case '\0': if (!logSpecifier<unsigned>(s, specifierString, size)) return false; break;
                default: break; // Invalid specifier is ignored and output as is
                }
            } break;
            case Specifier::String:
            {
                char * string = {}; std::size_t len = 0;
                if (!Log::logBuffer.loadString(nullptr, len)) return false;
                string = new char[len];
                if (!Log::logBuffer.loadString(string, len)) { delete[] string; return false; }
                if (sized) s.appendV(specifierString, len - 1, string); else s.appendV(specifierString, string);
                delete[] string;
            } break;
            case Specifier::Char:
            {
                if (!Log::logBuffer.load(type)) return false;
                s.appendV(specifierString, type);
            } break;
            case Specifier::Pointer:
            {
                const void * ptr = {};
                if (!Log::logBuffer.load(ptr)) return false;
                if (sized) s.appendV(specifierString, size, ptr); else s.appendV(specifierString, ptr);
            }
            case Specifier::Unknown: default: break; // Invalid specifier is ignored and output as is
            }
            str = spec+1;
        }
        // Finally append what remains here
        while (true) {
            const char * spec = nextSpecifier(str, true);
            if (spec) s.append(str, spec - str);
            else { s.append(str); break; }
            str = spec+1;
        }
        return true;
    }
}
