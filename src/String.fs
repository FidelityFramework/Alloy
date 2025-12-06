#nowarn "9"
namespace Alloy

open Alloy.Core
open Alloy.Numerics

/// <summary>
/// Provides zero-cost string operations with safe and efficient string manipulation
/// through statically resolved type parameters. All functions are exposed through the 
/// AutoOpen attribute, making them accessible when opening the Alloy namespace.
/// </summary>
[<AutoOpen>]
module String =
    [<Literal>]
    let private INT_MAX_VALUE = 2147483647

    let inline private parseDigit (c: char) : ValueOption<int> =
        if c >= '0' && c <= '9' then Some(int c - int '0')
        else None

    let inline private increment (i: int) : int =
        if i = INT_MAX_VALUE then i
        else add i 1

    /// <summary>
    /// Provides optimized string operations with .NET compatible signatures
    /// </summary>
    [<AbstractClass; Sealed>]
    type StringOps =
        /// <summary>Concatenates two strings, ensuring neither is null</summary>
        static member inline Concat(s1: string, s2: string) : string =
            let s1Safe = if isNull s1 then "" else s1
            let s2Safe = if isNull s2 then "" else s2
            
            let len1 = s1Safe.Length
            let len2 = s2Safe.Length
            let result = Array.zeroCreate<char> (add len1 len2)
            
            for i = 0 to len1 - 1 do
                result[i] <- s1Safe[i]
            
            for i = 0 to len2 - 1 do
                result[add i len1] <- s2Safe[i]
                
            new string(result)
            
        /// <summary>Concatenates three strings</summary>
        static member inline Concat(s1: string, s2: string, s3: string) : string =
            let s1Safe = if isNull s1 then "" else s1
            let s2Safe = if isNull s2 then "" else s2
            let s3Safe = if isNull s3 then "" else s3
            
            let len1 = s1Safe.Length
            let len2 = s2Safe.Length
            let len3 = s3Safe.Length
            let totalLen = add (add len1 len2) len3
            
            let result = Array.zeroCreate<char> totalLen
            
            for i = 0 to len1 - 1 do
                result[i] <- s1Safe[i]
            
            for i = 0 to len2 - 1 do
                result[add i len1] <- s2Safe[i]
                
            for i = 0 to len3 - 1 do
                result[add (add i len1) len2] <- s3Safe[i]
                
            new string(result)
            
        /// <summary>Concatenates four strings</summary>
        static member inline Concat(s1: string, s2: string, s3: string, s4: string) : string =
            let s1Safe = if isNull s1 then "" else s1
            let s2Safe = if isNull s2 then "" else s2
            let s3Safe = if isNull s3 then "" else s3
            let s4Safe = if isNull s4 then "" else s4
            
            let len1 = s1Safe.Length
            let len2 = s2Safe.Length
            let len3 = s3Safe.Length
            let len4 = s4Safe.Length
            let totalLen = add (add (add len1 len2) len3) len4
            
            let result = Array.zeroCreate<char> totalLen
            
            let mutable currentIndex = 0
            
            for i = 0 to len1 - 1 do
                result[currentIndex] <- s1Safe[i]
                currentIndex <- increment currentIndex
                
            for i = 0 to len2 - 1 do
                result[currentIndex] <- s2Safe[i]
                currentIndex <- increment currentIndex
                
            for i = 0 to len3 - 1 do
                result[currentIndex] <- s3Safe[i]
                currentIndex <- increment currentIndex
                
            for i = 0 to len4 - 1 do
                result[currentIndex] <- s4Safe[i]
                currentIndex <- increment currentIndex
                
            new string(result)
            
        /// <summary>Concatenates an array of strings</summary>
        static member inline Concat(values: string[]) : string =
            if isNull values || values.Length = 0 then ""
            elif values.Length = 1 then 
                if isNull values[0] then "" else values[0]
            else
                let mutable totalLength = 0
                for i = 0 to values.Length - 1 do
                    if not (isNull values[i]) then
                        totalLength <- add totalLength values[i].Length
                        
                let result = Array.zeroCreate<char> totalLength
                let mutable currentIndex = 0
                
                for i = 0 to values.Length - 1 do
                    if not (isNull values[i]) then
                        for j = 0 to values[i].Length - 1 do
                            result[currentIndex] <- values[i][j]
                            currentIndex <- increment currentIndex
                            
                new string(result)
        
        /// <summary>Concatenates a sequence of strings</summary>
        static member inline Concat(values: seq<string>) : string =
            if isNull values then ""
            else
                let arr = Seq.toArray values
                StringOps.Concat(arr)
                
        /// <summary>Joins multiple strings using a separator</summary>
        static member inline Join(separator: string, values: string[]) : string =
            if isNull values || values.Length = 0 then ""
            elif values.Length = 1 then
                if isNull values[0] then "" else values[0]
            else
                let separatorSafe = if isNull separator then "" else separator
                
                // Calculate total length
                let mutable totalLength = 0
                for i = 0 to values.Length - 1 do
                    if not (isNull values[i]) then
                        totalLength <- add totalLength values[i].Length
                
                // Add space for separators
                totalLength <- add totalLength (multiply separatorSafe.Length (subtract values.Length 1))
                
                let result = Array.zeroCreate<char> totalLength
                let mutable currentIndex = 0
                
                // Add first element
                if not (isNull values[0]) then
                    for j = 0 to values[0].Length - 1 do
                        result[currentIndex] <- values[0][j]
                        currentIndex <- increment currentIndex
                
                // Add separator and subsequent elements
                for i = 1 to values.Length - 1 do
                    // Add separator
                    for j = 0 to separatorSafe.Length - 1 do
                        result[currentIndex] <- separatorSafe[j]
                        currentIndex <- increment currentIndex
                    
                    // Add element
                    if not (isNull values[i]) then
                        for j = 0 to values[i].Length - 1 do
                            result[currentIndex] <- values[i][j]
                            currentIndex <- increment currentIndex
                
                new string(result)
                
        /// <summary>Joins a sequence of strings using a separator</summary>
        static member inline Join(separator: string, values: seq<string>) : string =
            if isNull values then ""
            else
                let arr = Seq.toArray values
                StringOps.Join(separator, arr)
                
        /// <summary>Joins a sequence of objects using a separator</summary>
        static member inline Join<'T>(separator: string, values: seq<'T>) : string =
            if isNull values then ""
            else
                let mutable stringValues = [||]
                for value in values do
                    let strValue = 
                        if box value = null then ""
                        else string value
                    stringValues <- Array.append stringValues [|strValue|]
                StringOps.Join(separator, stringValues)

    // --------------------------
    // Public API
    // --------------------------
    
    /// <summary>Gets a value indicating whether the string is a valid number</summary>
    /// <param name="s">The string to check</param>
    /// <returns>True if the string is a valid number</returns>
    let inline isNumber (s: string) : bool =
        if isNull s || s.Length = 0 then false
        else
            let mutable valid = true
            let mutable i = if s[0] = '-' || s[0] = '+' then 1 else 0
            let mutable hasDecimal = false
            
            if i = s.Length then false
            else
                while i < s.Length && valid do
                    if s[i] = '.' && not hasDecimal then
                        hasDecimal <- true
                        i <- increment i
                    else
                        match parseDigit s[i] with
                        | Some _ -> i <- increment i
                        | None -> valid <- false
                        
                valid && (if hasDecimal then i > (if s[0] = '-' || s[0] = '+' then 2 else 1) else i > (if s[0] = '-' || s[0] = '+' then 1 else 0))
    
    /// <summary>Concatenates two strings, ensuring neither is null</summary>
    /// <param name="s1">The first string</param>
    /// <param name="s2">The second string</param>
    /// <returns>The concatenated string</returns>
    let inline concat (s1: string) (s2: string) : string = StringOps.Concat(s1, s2)

    /// <summary>Concatenates three strings</summary>
    /// <param name="s1">The first string</param>
    /// <param name="s2">The second string</param>
    /// <param name="s3">The third string</param>
    /// <returns>The concatenated string</returns>
    let inline concat3 (s1: string) (s2: string) (s3: string) : string = 
        StringOps.Concat(s1, s2, s3)

    /// <summary>Concatenates four strings</summary>
    /// <param name="s1">The first string</param>
    /// <param name="s2">The second string</param>
    /// <param name="s3">The third string</param>
    /// <param name="s4">The fourth string</param>
    /// <returns>The concatenated string</returns>
    let inline concat4 (s1: string) (s2: string) (s3: string) (s4: string) : string = 
        StringOps.Concat(s1, s2, s3, s4)

    /// <summary>Concatenates an array of strings</summary>
    /// <param name="values">An array of strings to concatenate</param>
    /// <returns>The concatenated string</returns>
    let inline concatArray (values: string[]) : string = 
        StringOps.Concat(values)

    /// <summary>Concatenates a sequence of strings</summary>
    /// <param name="values">A sequence of strings to concatenate</param>
    /// <returns>The concatenated string</returns>
    let inline concatSeq (values: seq<string>) : string = 
        StringOps.Concat(values)

    /// <summary>Concatenates multiple strings using a separator</summary>
    /// <param name="separator">The string to use as a separator</param>
    /// <param name="values">A sequence of strings to join</param>
    /// <returns>A string consisting of the concatenated elements with separators between them</returns>
    let inline join (separator: string) (values: string[]) : string =
        StringOps.Join(separator, values)
        
    /// <summary>Concatenates multiple strings using a separator</summary>
    /// <param name="separator">The string to use as a separator</param>
    /// <param name="values">A sequence of strings to join</param>
    /// <returns>A string consisting of the concatenated elements with separators between them</returns>
    let inline joinSeq (separator: string) (values: seq<string>) : string =
        StringOps.Join(separator, values)
        
    /// <summary>Concatenates multiple values using a separator</summary>
    /// <param name="separator">The string to use as a separator</param>
    /// <param name="values">A sequence of values to join</param>
    /// <returns>A string consisting of the concatenated elements with separators between them</returns>
    let inline joinGeneric<'T> (separator: string) (values: seq<'T>) : string =
        StringOps.Join(separator, values)
    
    /// <summary>Safely gets the character at the specified position</summary>
    /// <param name="index">The index of the character to get</param>
    /// <param name="s">The source string</param>
    /// <returns>The character at the specified position, or ValueNone if invalid</returns>
    let inline charAt (index: int) (s: string) : ValueOption<char> =
        if isNull s || index < 0 || index >= s.Length then None<char>
        else Some s[index]
    
    /// <summary>Checks if a character is a digit</summary>
    /// <param name="c">The character to check</param>
    /// <returns>True if the character is a digit</returns>
    let inline isDigit (c: char) : bool =
        c >= '0' && c <= '9'
    
    /// <summary>Checks if a string consists only of digits</summary>
    /// <param name="s">The string to check</param>
    /// <returns>True if the string consists only of digits</returns>
    let inline isDigitsOnly (s: string) : bool =
        if isNull s || s.Length = 0 then false
        else
            let mutable allDigits = true
            let mutable i = 0
            while i < s.Length && allDigits do
                if not (isDigit s[i]) then
                    allDigits <- false
                i <- increment i
            allDigits
    
    /// <summary>Returns the length of a string</summary>
    /// <param name="s">The input string</param>
    /// <returns>The length of the string, or 0 if null</returns>
    let inline length (s: string) : int =
        if isNull s then 0 else s.Length
    
    /// <summary>Checks if a string is empty</summary>
    /// <param name="s">The string to check</param>
    /// <returns>True if the string is null or empty</returns>
    let inline isEmpty (s: string) : bool =
        isNull s || s.Length = 0
    
    /// <summary>Checks if a string is null or empty</summary>
    /// <param name="s">The string to check</param>
    /// <returns>True if the string is null or empty</returns>
    let inline isNullOrEmpty (s: string) : bool =
        isNull s || s.Length = 0
    
    /// <summary>Checks if character is whitespace</summary>
    /// <param name="c">The character to check</param>
    /// <returns>True if the character is whitespace</returns>
    let inline private isWhiteSpace (c: char) : bool =
        c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = '\f' || c = '\v'
    
    /// <summary>Checks if a string is null, empty, or consists only of white-space characters</summary>
    /// <param name="s">The string to check</param>
    /// <returns>True if the string is null, empty, or whitespace</returns>
    let inline isNullOrWhiteSpace (s: string) : bool =
        if isNull s || s.Length = 0 then true
        else
            let mutable onlyWhitespace = true
            let mutable i = 0
            while i < s.Length && onlyWhitespace do
                if not (isWhiteSpace s[i]) then
                    onlyWhitespace <- false
                i <- add i 1
            onlyWhitespace
    
    /// <summary>Calculates how many characters are available from a starting position</summary>
    /// <param name="s">The source string</param>
    /// <param name="startIndex">The starting index</param>
    /// <returns>The number of available characters</returns>
    let inline private availableChars (s: string) (startIndex: int) : int =
        if startIndex >= s.Length then 0
        else
            let mutable count = 0
            let mutable i = startIndex
            while i < s.Length do
                count <- increment count
                i <- increment i
            count
    
    /// <summary>Gets a substring from a string</summary>
    /// <param name="startIndex">The zero-based starting position</param>
    /// <param name="length">The number of characters to extract</param>
    /// <param name="s">The source string</param>
    /// <returns>The substring, or empty string if parameters are invalid</returns>
    let inline substring (startIndex: int) (length: int) (s: string) : string =
        if isNull s then ""
        elif startIndex < 0 || startIndex >= s.Length then ""
        elif length <= 0 then ""
        else 
            let available = availableChars s startIndex
            let safeLength = min length available
            
            if safeLength <= 0 then ""
            else
                let result = Array.zeroCreate<char> safeLength
                for i = 0 to safeLength - 1 do
                    result[i] <- s[add startIndex i]
                new string(result)
    
    /// <summary>Gets a substring from a string to the end</summary>
    /// <param name="startIndex">The zero-based starting position</param>
    /// <param name="s">The source string</param>
    /// <returns>The substring, or empty string if parameters are invalid</returns>
    let inline substringFrom (startIndex: int) (s: string) : string =
        if isNull s then ""
        elif startIndex < 0 || startIndex >= s.Length then ""
        else
            let length = subtract s.Length startIndex
            substring startIndex length s
    
    /// <summary>Determines whether a string contains the specified substring</summary>
    /// <param name="value">The string to seek</param>
    /// <param name="s">The string to search</param>
    /// <returns>True if the value is found within the string</returns>
    let inline contains (value: string) (s: string) : bool =
        if isNull s then false
        elif isNull value then false
        elif value.Length = 0 then true
        elif value.Length > s.Length then false
        else
            let mutable found = false
            let mutable i = 0
            while i <= s.Length - value.Length && not found do
                let mutable matches = true
                let mutable j = 0
                
                while j < value.Length && matches do
                    if s[i + j] <> value[j] then matches <- false
                    j <- increment j
                    
                if matches then
                    found <- true
                i <- increment i
            found
    
    /// <summary>Splits a string using the specified separator</summary>
    /// <param name="separator">The character that delimits substrings</param>
    /// <param name="s">The string to split</param>
    /// <returns>An array of substrings</returns>
    let inline split (separator: char) (s: string) : string[] =
        if isNull s then [||]
        else
            let mutable count = 1
            for i = 0 to s.Length - 1 do
                if s[i] = separator then count <- increment count
            
            let result = Array.zeroCreate count
            let mutable startIdx = 0
            let mutable resultIdx = 0
            
            for i = 0 to s.Length - 1 do
                if s[i] = separator then
                    result[resultIdx] <- substring startIdx (i - startIdx) s
                    resultIdx <- increment resultIdx
                    startIdx <- i + 1
            
            if startIdx <= s.Length then
                result[resultIdx] <- substring startIdx (s.Length - startIdx) s
            
            result
    
    /// <summary>Splits a string using multiple separators</summary>
    /// <param name="separators">An array of characters that delimit substrings</param>
    /// <param name="s">The string to split</param>
    /// <returns>An array of substrings</returns>
    let inline splitMany (separators: char[]) (s: string) : string[] =
        if isNull s then [||]
        elif isNull separators || separators.Length = 0 then [|s|]
        else
            let isSeparator (c: char) =
                let mutable result = false
                for i = 0 to separators.Length - 1 do
                    if separators[i] = c then result <- true
                result
            
            let mutable count = 1
            for i = 0 to s.Length - 1 do
                if isSeparator s[i] then count <- increment count
            
            let result = Array.zeroCreate count
            let mutable startIdx = 0
            let mutable resultIdx = 0
            
            for i = 0 to s.Length - 1 do
                if isSeparator s[i] then
                    result[resultIdx] <- substring startIdx (i - startIdx) s
                    resultIdx <- increment resultIdx
                    startIdx <- i + 1
            
            if startIdx <= s.Length then
                result[resultIdx] <- substring startIdx (s.Length - startIdx) s
            
            result
    
    /// <summary>Removes all leading and trailing white-space characters</summary>
    /// <param name="s">The string to trim</param>
    /// <returns>The trimmed string</returns>
    let inline trim (s: string) : string =
        if isNull s then ""
        else
            let mutable startIdx = 0
            let mutable endIdx = s.Length - 1
            
            while startIdx <= endIdx && isWhiteSpace s[startIdx] do
                startIdx <- increment startIdx
                
            while endIdx >= startIdx && isWhiteSpace s[endIdx] do
                endIdx <- subtract endIdx 1
                
            if startIdx > endIdx then ""
            else substring startIdx (add (subtract endIdx startIdx) 1) s
    
    /// <summary>Removes all leading white-space characters</summary>
    /// <param name="s">The string to trim</param>
    /// <returns>The trimmed string</returns>
    let inline trimStart (s: string) : string =
        if isNull s then ""
        else
            let mutable startIdx = 0
            while startIdx < s.Length && isWhiteSpace s[startIdx] do
                startIdx <- increment startIdx
                
            if startIdx = s.Length then ""
            else substring startIdx (subtract s.Length startIdx) s
    
    /// <summary>Removes all trailing white-space characters</summary>
    /// <param name="s">The string to trim</param>
    /// <returns>The trimmed string</returns>
    let inline trimEnd (s: string) : string =
        if isNull s then ""
        else
            let mutable endIdx = subtract s.Length 1
            while endIdx >= 0 && isWhiteSpace s[endIdx] do
                endIdx <- subtract endIdx 1
                
            if endIdx < 0 then ""
            else substring 0 (add endIdx 1) s
    
    /// <summary>Replaces all occurrences of a specified string with another specified string</summary>
    /// <param name="oldValue">The string to be replaced</param>
    /// <param name="newValue">The string to replace occurrences of oldValue</param>
    /// <param name="s">The source string</param>
    /// <returns>A string with all occurrences of oldValue replaced by newValue</returns>
    let inline replace (oldValue: string) (newValue: string) (s: string) : string =
        if isNull s then ""
        elif isNull oldValue || oldValue.Length = 0 then s
        else
            let mutable result = ""
            let mutable i = 0
            
            while i < s.Length do
                let mutable isMatch = i + oldValue.Length <= s.Length
                let mutable j = 0
                
                while j < oldValue.Length && isMatch do
                    if s[i + j] <> oldValue[j] then isMatch <- false
                    j <- increment j
                    
                if isMatch then
                    result <- concat result newValue
                    i <- add i oldValue.Length
                else
                    result <- concat result (string s[i])
                    i <- increment i
                    
            result
    
    /// <summary>Determines whether a string starts with the specified prefix</summary>
    /// <param name="prefix">The prefix to test</param>
    /// <param name="s">The string to check</param>
    /// <returns>True if the string starts with the specified prefix</returns>
    let inline startsWith (prefix: string) (s: string) : bool =
        if isNull s then false
        elif isNull prefix then false
        elif prefix.Length > s.Length then false
        else
            let mutable matches = true
            let mutable i = 0
            
            while i < prefix.Length && matches do
                if s[i] <> prefix[i] then matches <- false
                i <- increment i
                
            matches
    
    /// <summary>Determines whether a string ends with the specified suffix</summary>
    /// <param name="suffix">The suffix to test</param>
    /// <param name="s">The string to check</param>
    /// <returns>True if the string ends with the specified suffix</returns>
    let inline endsWith (suffix: string) (s: string) : bool =
        if isNull s then false
        elif isNull suffix then false
        elif suffix.Length > s.Length then false
        else
            let mutable matches = true
            let mutable i = 0
            
            while i < suffix.Length && matches do
                if s[s.Length - suffix.Length + i] <> suffix[i] then matches <- false
                i <- increment i
                
            matches
    
    /// <summary>Convert character to lowercase</summary>
    /// <param name="c">The character to convert</param>
    /// <returns>The lowercase equivalent of the character</returns>
    let inline private charToLower (c: char) : char =
        if c >= 'A' && c <= 'Z' then
            char ((int c) + 32)
        else c
    
    /// <summary>Convert character to uppercase</summary>
    /// <param name="c">The character to convert</param>
    /// <returns>The uppercase equivalent of the character</returns>
    let inline private charToUpper (c: char) : char =
        if c >= 'a' && c <= 'z' then
            char ((int c) - 32)
        else c
    
    /// <summary>Converts a string to lowercase</summary>
    /// <param name="s">The string to convert</param>
    /// <returns>A copy of the string converted to lowercase</returns>
    let inline toLower (s: string) : string =
        if isNull s then ""
        else
            let result = Array.zeroCreate s.Length
            for i = 0 to s.Length - 1 do
                result[i] <- charToLower s[i]
            new string(result)
    
    /// <summary>Converts a string to uppercase</summary>
    /// <param name="s">The string to convert</param>
    /// <returns>A copy of the string converted to uppercase</returns>
    let inline toUpper (s: string) : string =
        if isNull s then ""
        else
            let result = Array.zeroCreate s.Length
            for i = 0 to s.Length - 1 do
                result[i] <- charToUpper s[i]
            new string(result)
    
    /// <summary>Returns the index of the first occurrence of the specified substring</summary>
    /// <param name="value">The string to seek</param>
    /// <param name="s">The string to search</param>
    /// <returns>The index of the first occurrence, or -1 if not found</returns>
    let inline indexOf (value: string) (s: string) : int =
        if isNull s then -1
        elif isNull value || value.Length = 0 then 0
        elif value.Length > s.Length then -1
        else
            let mutable index = -1
            let mutable i = 0
            
            while i <= s.Length - value.Length && index = -1 do
                let mutable matches = true
                let mutable j = 0
                
                while j < value.Length && matches do
                    if s[i + j] <> value[j] then matches <- false
                    j <- increment j
                    
                if matches then index <- i
                i <- increment i
                
            index
    
    /// <summary>Returns the index of the last occurrence of the specified substring</summary>
    /// <param name="value">The string to seek</param>
    /// <param name="s">The string to search</param>
    /// <returns>The index of the last occurrence, or -1 if not found</returns>
    let inline lastIndexOf (value: string) (s: string) : int =
        if isNull s then -1
        elif isNull value || value.Length = 0 then if s.Length = 0 then 0 else s.Length - 1
        elif value.Length > s.Length then -1
        else
            let mutable index = -1
            let mutable i = s.Length - value.Length
            
            while i >= 0 && index = -1 do
                let mutable matches = true
                let mutable j = 0
                
                while j < value.Length && matches do
                    if s[i + j] <> value[j] then matches <- false
                    j <- increment j
                    
                if matches then index <- i
                i <- subtract i 1
                
            index
    
    /// <summary>Returns the index of the first occurrence of any character in the specified array</summary>
    /// <param name="anyOf">Characters to seek</param>
    /// <param name="s">The string to search</param>
    /// <returns>The index of the first occurrence of any character, or -1 if not found</returns>
    let inline indexOfAny (anyOf: char[]) (s: string) : int =
        if isNull s then -1
        elif isNull anyOf || anyOf.Length = 0 then -1
        else
            let mutable index = -1
            let mutable i = 0
            
            while i < s.Length && index = -1 do
                let mutable found = false
                let mutable j = 0
                
                while j < anyOf.Length && not found do
                    if s[i] = anyOf[j] then found <- true
                    j <- increment j
                    
                if found then index <- i
                i <- increment i
                
            index
    
    /// <summary>Converts a string to a char array</summary>
    /// <param name="s">The string to convert</param>
    /// <returns>A char array containing the characters from the string</returns>
    let inline toCharArray (s: string) : char[] =
        if isNull s then [||]
        else
            let result = Array.zeroCreate s.Length
            for i = 0 to s.Length - 1 do
                result[i] <- s[i]
            result
    
    /// <summary>Checks if a string is equal to another string, ignoring case</summary>
    /// <param name="value">The string to compare</param>
    /// <param name="s">The string to check</param>
    /// <returns>True if the strings are equal, ignoring case</returns>
    let inline equalsIgnoreCase (value: string) (s: string) : bool =
        if isNull s then isNull value
        elif isNull value then false
        elif s.Length <> value.Length then false
        else
            let mutable equal = true
            for i = 0 to s.Length - 1 do
                if charToLower s[i] <> charToLower value[i] then
                    equal <- false
            equal
    
    /// <summary>Gets a value indicating whether the string is a valid integer</summary>
    /// <param name="s">The string to check</param>
    /// <returns>True if the string is a valid integer</returns>
    let inline isInteger (s: string) : bool =
        if isNull s || s.Length = 0 then false
        else
            let mutable valid = true
            let mutable i = if s[0] = '-' || s[0] = '+' then 1 else 0
            
            if i = s.Length then false
            else
                while i < s.Length && valid do
                    match parseDigit s[i] with
                    | Some _ -> i <- increment i
                    | None -> valid <- false
                        
                valid

    /// <summary>Creates a new string by repeating a character a specified number of times</summary>
    /// <param name="count">The number of times to repeat the character</param>
    /// <param name="c">The character to repeat</param>
    /// <returns>A new string consisting of the repeated character</returns>
    let inline replicate (count: int) (c: char) : string =
        if count <= 0 then ""
        else
            let chars = Array.create count c
            new string(chars)

    /// <summary>Pads a string on the left with a specified character to reach a specified length</summary>
    /// <param name="paddingChar">The character to use for padding</param>
    /// <param name="totalWidth">The desired total length of the resulting string</param>
    /// <param name="s">The string to pad</param>
    /// <returns>The padded string</returns>
    let inline padLeft (paddingChar: char) (totalWidth: int) (s: string) : string =
        if isNull s then ""
        elif s.Length >= totalWidth then s
        else
            let padding = replicate (subtract totalWidth s.Length) paddingChar
            concat padding s

    /// <summary>Pads a string on the right with a specified character to reach a specified length</summary>
    /// <param name="paddingChar">The character to use for padding</param>
    /// <param name="totalWidth">The desired total length of the resulting string</param>
    /// <param name="s">The string to pad</param>
    /// <returns>The padded string</returns>
    let inline padRight (paddingChar: char) (totalWidth: int) (s: string) : string =
        if isNull s then ""
        elif s.Length >= totalWidth then s
        else
            let padding = replicate (subtract totalWidth s.Length) paddingChar
            concat s padding
            
    // Compatibility function for backward compatibility
    /// <summary>Concatenates multiple strings using a separator</summary>
    /// <param name="separator">The string to use as a separator</param>
    /// <param name="values">A sequence of strings to join</param>
    /// <returns>A string consisting of the concatenated elements with separators between them</returns>
    let inline concatMany (separator: string) (values: seq<string>) : string =
        joinSeq separator values