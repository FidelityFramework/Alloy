#nowarn "9"
namespace Alloy

/// <summary>
/// Pure F# UTF-8 encoding/decoding implementation
/// </summary>
module Utf8 =
    /// <summary>
    /// Encodes a string to UTF-8 bytes
    /// </summary>
    /// <param name="s">The string to encode</param>
    /// <returns>A byte array containing the UTF-8 encoded string</returns>
    let getBytes (s: string) : byte[] =
        if s = null || s.Length = 0 then
            [||]
        else
            let estimatedSize = s.Length * 4 // Worst case: 4 bytes per char
            let result = ResizeArray<byte>(estimatedSize)
            
            for i = 0 to s.Length - 1 do
                let c = int (s[i])
                if c < 0x80 then
                    // 1-byte sequence: 0xxxxxxx
                    result.Add(byte c)
                elif c < 0x800 then
                    // 2-byte sequence: 110xxxxx 10xxxxxx
                    result.Add(byte (0xC0 ||| (c >>> 6)))
                    result.Add(byte (0x80 ||| (c &&& 0x3F)))
                elif c < 0x10000 then
                    // 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx
                    result.Add(byte (0xE0 ||| (c >>> 12)))
                    result.Add(byte (0x80 ||| ((c >>> 6) &&& 0x3F)))
                    result.Add(byte (0x80 ||| (c &&& 0x3F)))
                else
                    // 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
                    result.Add(byte (0xF0 ||| (c >>> 18)))
                    result.Add(byte (0x80 ||| ((c >>> 12) &&& 0x3F)))
                    result.Add(byte (0x80 ||| ((c >>> 6) &&& 0x3F)))
                    result.Add(byte (0x80 ||| (c &&& 0x3F)))
                    
            result.ToArray()
    
    /// <summary>
    /// Decodes UTF-8 bytes to string
    /// </summary>
    /// <param name="bytes">The byte array containing UTF-8 encoded data</param>
    /// <returns>The decoded string</returns>
    let getString (bytes: byte[]) : string =
        if bytes = null || bytes.Length = 0 then
            ""
        else
            let chars = ResizeArray<char>()
            let mutable i = 0
            
            while i < bytes.Length do
                let b = int bytes[i]
                
                if b < 0x80 then
                    // 1-byte sequence
                    chars.Add(char b)
                    i <- i + 1
                elif b < 0xE0 then
                    // 2-byte sequence
                    if i + 1 < bytes.Length then
                        let b2 = int bytes[i + 1]
                        let c = ((b &&& 0x1F) <<< 6) ||| (b2 &&& 0x3F)
                        chars.Add(char c)
                        i <- i + 2
                    else 
                        // Invalid sequence, skip
                        i <- i + 1
                elif b < 0xF0 then
                    // 3-byte sequence
                    if i + 2 < bytes.Length then
                        let b2 = int bytes[i + 1]
                        let b3 = int bytes[i + 2]
                        let c = ((b &&& 0x0F) <<< 12) ||| ((b2 &&& 0x3F) <<< 6) ||| (b3 &&& 0x3F)
                        chars.Add(char c)
                        i <- i + 3
                    else 
                        // Invalid sequence, skip
                        i <- i + 1
                else
                    // 4-byte sequence
                    if i + 3 < bytes.Length then
                        let b2 = int bytes[i + 1]
                        let b3 = int bytes[i + 2]
                        let b4 = int bytes[i + 3]
                        let c = ((b &&& 0x07) <<< 18) ||| ((b2 &&& 0x3F) <<< 12) ||| 
                                ((b3 &&& 0x3F) <<< 6) ||| (b4 &&& 0x3F)
                        // Handle surrogate pairs for characters outside BMP
                        if c >= 0x10000 then
                            let highSurrogate = char (0xD800 ||| ((c - 0x10000) >>> 10))
                            let lowSurrogate = char (0xDC00 ||| ((c - 0x10000) &&& 0x3FF))
                            chars.Add(highSurrogate)
                            chars.Add(lowSurrogate)
                        else
                            chars.Add(char c)
                        i <- i + 4
                    else 
                        // Invalid sequence, skip
                        i <- i + 1
                    
            new string(chars.ToArray())
            
    /// <summary>
    /// Calculates the UTF-8 encoded length of a string
    /// </summary>
    /// <param name="s">The string to measure</param>
    /// <returns>The number of bytes needed for UTF-8 encoding</returns>
    let calculateEncodedLength (s: string) : int =
        if s = null || s.Length = 0 then
            0
        else
            let mutable length = 0
            
            for i = 0 to s.Length - 1 do
                let c = int (s[i])
                if c < 0x80 then
                    // 1-byte sequence
                    length <- length + 1
                elif c < 0x800 then
                    // 2-byte sequence
                    length <- length + 2
                elif c < 0x10000 then
                    // 3-byte sequence
                    length <- length + 3
                else
                    // 4-byte sequence
                    length <- length + 4
            
            length