#nowarn "9"
namespace Alloy

open FSharp.NativeInterop

/// Zero-allocation text processing for native compilation.
///
/// NOTE: String type with native semantics (UTF-8 fat pointer) is provided by FNCS.
/// In native compilation, string has: Pointer (nativeptr<byte>), Length (int)
///
/// All operations use direct pointer access - no BCL dependencies.
module Text =

    // ═══════════════════════════════════════════════════════════════════
    // UTF-8 Encoding/Decoding
    // ═══════════════════════════════════════════════════════════════════

    module UTF8 =
        /// Encodes a single Unicode codepoint to UTF-8 bytes.
        /// Returns the number of bytes written (1-4).
        let inline encodeCodepoint (codepoint: int) (buffer: nativeptr<byte>) (offset: int) : int =
            if codepoint < 0x80 then
                // ASCII (1 byte)
                NativePtr.set buffer offset (byte codepoint)
                1
            elif codepoint < 0x800 then
                // 2-byte sequence
                NativePtr.set buffer offset (0xC0uy ||| byte (codepoint >>> 6))
                NativePtr.set buffer (offset + 1) (0x80uy ||| byte (codepoint &&& 0x3F))
                2
            elif codepoint < 0x10000 then
                // 3-byte sequence
                NativePtr.set buffer offset (0xE0uy ||| byte (codepoint >>> 12))
                NativePtr.set buffer (offset + 1) (0x80uy ||| byte ((codepoint >>> 6) &&& 0x3F))
                NativePtr.set buffer (offset + 2) (0x80uy ||| byte (codepoint &&& 0x3F))
                3
            else
                // 4-byte sequence
                NativePtr.set buffer offset (0xF0uy ||| byte (codepoint >>> 18))
                NativePtr.set buffer (offset + 1) (0x80uy ||| byte ((codepoint >>> 12) &&& 0x3F))
                NativePtr.set buffer (offset + 2) (0x80uy ||| byte ((codepoint >>> 6) &&& 0x3F))
                NativePtr.set buffer (offset + 3) (0x80uy ||| byte (codepoint &&& 0x3F))
                4

        /// Decodes a UTF-8 byte sequence to a Unicode codepoint.
        /// Returns (codepoint, bytes_consumed).
        let inline decodeCodepoint (buffer: nativeptr<byte>) (offset: int) (maxLen: int) : struct (int * int) =
            if maxLen <= 0 then
                struct (0, 0)
            else
                let b0 = NativePtr.get buffer offset
                if b0 < 0x80uy then
                    // ASCII
                    struct (int b0, 1)
                elif b0 < 0xC0uy then
                    // Invalid continuation byte at start
                    struct (0xFFFD, 1)  // Replacement character
                elif b0 < 0xE0uy then
                    // 2-byte sequence
                    if maxLen < 2 then struct (0xFFFD, 1)
                    else
                        let b1 = NativePtr.get buffer (offset + 1)
                        let cp = ((int (b0 &&& 0x1Fuy)) <<< 6) ||| (int (b1 &&& 0x3Fuy))
                        struct (cp, 2)
                elif b0 < 0xF0uy then
                    // 3-byte sequence
                    if maxLen < 3 then struct (0xFFFD, 1)
                    else
                        let b1 = NativePtr.get buffer (offset + 1)
                        let b2 = NativePtr.get buffer (offset + 2)
                        let cp = ((int (b0 &&& 0x0Fuy)) <<< 12) |||
                                 ((int (b1 &&& 0x3Fuy)) <<< 6) |||
                                 (int (b2 &&& 0x3Fuy))
                        struct (cp, 3)
                else
                    // 4-byte sequence
                    if maxLen < 4 then struct (0xFFFD, 1)
                    else
                        let b1 = NativePtr.get buffer (offset + 1)
                        let b2 = NativePtr.get buffer (offset + 2)
                        let b3 = NativePtr.get buffer (offset + 3)
                        let cp = ((int (b0 &&& 0x07uy)) <<< 18) |||
                                 ((int (b1 &&& 0x3Fuy)) <<< 12) |||
                                 ((int (b2 &&& 0x3Fuy)) <<< 6) |||
                                 (int (b3 &&& 0x3Fuy))
                        struct (cp, 4)

        /// Returns the byte length of a native UTF-8 string.
        /// For actual codepoint counting, use calculateCodepointCount from Utf8 module.
        let inline countCodepoints (str: string) : int =
            str.Length  // Native string length IS byte count (UTF-8)

        /// Gets UTF-8 bytes from a string.
        /// Native strings are already UTF-8 encoded.
        let inline getBytes (str: string) : byte[] =
            let arr = Array.zeroCreate<byte> str.Length
            if str.Length > 0 then
                NativePtr.copy (NativePtr.ofNativeInt<byte> (NativePtr.toNativeInt &&arr.[0])) str.Pointer str.Length
            arr

        /// Creates a string from UTF-8 bytes.
        /// Creates native string from byte array pointer.
        let inline fromBytes (bytes: byte[]) : string =
            if bytes.Length = 0 then ""
            else NativeStr.fromPointer (NativePtr.ofNativeInt<byte> (NativePtr.toNativeInt &&bytes.[0])) bytes.Length

    // ═══════════════════════════════════════════════════════════════════
    // String Operations
    // FNCS provides string with Pointer/Length. These are library functions.
    // ═══════════════════════════════════════════════════════════════════

    module Str =
        /// Returns true if the string is empty.
        /// Native strings are never null, only check length.
        let inline isEmpty (s: string) : bool = s.Length = 0

        /// Returns the byte length of the string (UTF-8).
        /// Native strings are already UTF-8, so Length IS byte length.
        let inline byteLength (s: string) : int = s.Length

        /// Returns the character length of the string.
        let inline length (s: string) : int = s.Length

        /// Compares two strings for equality.
        let inline equals (a: string) (b: string) : bool = a = b

        /// Compares two strings lexicographically (native byte comparison).
        /// Returns: negative if a < b, zero if a = b, positive if a > b.
        let inline compare (a: string) (b: string) : int =
            let len1, len2 = a.Length, b.Length
            let minLen = if len1 < len2 then len1 else len2
            let mutable i = 0
            let mutable result = 0
            while result = 0 && i < minLen do
                let b1 = int (NativePtr.get a.Pointer i)
                let b2 = int (NativePtr.get b.Pointer i)
                result <- b1 - b2
                i <- i + 1
            if result <> 0 then result
            elif len1 < len2 then -1
            elif len1 > len2 then 1
            else 0

        /// Checks if a string starts with a prefix.
        let inline startsWith (prefix: string) (s: string) : bool =
            s.StartsWith(prefix)

        /// Checks if a string ends with a suffix.
        let inline endsWith (suffix: string) (s: string) : bool =
            s.EndsWith(suffix)

        /// Creates a substring of the string.
        let inline slice (start: int) (len: int) (s: string) : string =
            if start < 0 || start >= s.Length then ""
            else
                let actualLen = if start + len > s.Length then s.Length - start else len
                s.Substring(start, actualLen)

        /// Finds the first occurrence of a substring.
        /// Returns the index, or -1 if not found.
        let inline indexOf (needle: string) (haystack: string) : int =
            haystack.IndexOf(needle)

        /// Counts occurrences of a substring.
        let inline countOccurrences (needle: string) (haystack: string) : int =
            if needle.Length = 0 then 0
            else
                let mutable count = 0
                let mutable startIndex = 0
                while startIndex < haystack.Length do
                    let foundIndex = haystack.IndexOf(needle, startIndex)
                    if foundIndex >= 0 then
                        count <- count + 1
                        startIndex <- foundIndex + needle.Length
                    else
                        startIndex <- haystack.Length
                count

        /// Concatenates two strings.
        let inline concat (a: string) (b: string) : string =
            a + b

        /// Replaces all occurrences of oldValue with newValue.
        let inline replace (original: string) (oldValue: string) (newValue: string) : string =
            original.Replace(oldValue, newValue)

    // ═══════════════════════════════════════════════════════════════════
    // Integer to String Conversion
    // ═══════════════════════════════════════════════════════════════════

    module Format =
        /// Converts a 32-bit signed integer to a string.
        let inline int32ToString (value: int) : string =
            string value

        /// Converts a 64-bit signed integer to a string.
        let inline int64ToString (value: int64) : string =
            string value

        /// Converts a 32-bit unsigned integer to hexadecimal.
        let inline uint32ToHex (value: uint32) (uppercase: bool) : string =
            if uppercase then
                value.ToString("X")
            else
                value.ToString("x")

        /// Converts a 64-bit unsigned integer to hexadecimal.
        let inline uint64ToHex (value: uint64) (uppercase: bool) : string =
            if uppercase then
                value.ToString("X")
            else
                value.ToString("x")

        /// Creates a string of repeated characters (ASCII only).
        let inline repeatChar (c: char) (count: int) : string =
            if count <= 0 then ""
            else
                let buffer = NativePtr.stackalloc<byte> count
                NativePtr.fill buffer (byte c) count
                NativeStr.fromPointer buffer count

        /// Converts an integer to a string with padding.
        let inline int32ToStringPadded (value: int) (width: int) (padChar: char) : string =
            let s = string value
            if s.Length >= width then s
            else repeatChar padChar (width - s.Length) + s
