#nowarn "9"
namespace Alloy

open Alloy

/// <summary>
/// Units of measure for BAREWire types
/// </summary>
[<AutoOpen>]
module Measures =
    /// <summary>
    /// Marker for byte offset within a buffer
    /// </summary>
    [<Measure>] type offset

    /// <summary>
    /// Marker for byte count
    /// </summary>
    [<Measure>] type bytes
    
    /// <summary>
    /// Marker for a typed memory region
    /// </summary>
    [<Measure>] type region
    
    /// <summary>
    /// Base marker for schema types
    /// </summary>
    [<Measure>] type schema
    
    /// <summary>
    /// Schema validation state
    /// </summary>
    [<Measure>] type validated
    
    /// <summary>
    /// Schema draft state (not yet validated)
    /// </summary>
    [<Measure>] type draft

/// <summary>
/// BARE primitive types
/// </summary>
type PrimitiveType =
    | UInt
    | Int
    | U8
    | U16
    | U32
    | U64
    | I8
    | I16
    | I32
    | I64
    | F32
    | F64
    | Bool
    | String
    | Data
    | FixedData of length:int
    | Void
    | Enum of values:Map<string, uint64>

/// <summary>
/// BARE aggregate types
/// </summary>
type AggregateType =
    | Optional of Type
    | List of Type
    | FixedList of Type * length:int  
    | Map of keyType:Type * valueType:Type
    | Union of cases:Map<uint, Type>
    | Struct of fields:StructField list

/// <summary>
/// A field in a BARE struct
/// </summary>
and [<Struct>] StructField = {
    /// <summary>
    /// The name of the field
    /// </summary>
    Name: string
    
    /// <summary>
    /// The type of the field
    /// </summary>
    Type: Type
}

/// <summary>
/// A BARE type (either primitive or aggregate)
/// </summary>
and Type =
    | Primitive of PrimitiveType
    | Aggregate of AggregateType
    | UserDefined of string

/// <summary>
/// Size information for a BARE type
/// </summary>
[<Struct>]
type Size = {
    /// <summary>
    /// The minimum size in bytes
    /// </summary>
    Min: int<bytes>
    
    /// <summary>
    /// The maximum size in bytes (None if variable-length)
    /// </summary>
    Max: ValueOption<int<bytes>>
    
    /// <summary>
    /// Whether the size is fixed (Min = Max)
    /// </summary>
    IsFixed: bool
}

/// <summary>
/// Alignment requirements for a type
/// </summary>
[<Struct>]
type Alignment = {
    /// <summary>
    /// Alignment in bytes (must be a power of 2)
    /// </summary>
    Value: int<bytes>
}

/// <summary>
/// A schema definition with type safety
/// </summary>
/// <typeparam name="'state">The validation state of the schema</typeparam>
[<Struct>]
type SchemaDefinition<[<Measure>] 'state> = {
    /// <summary>
    /// The types defined in this schema
    /// </summary>
    Types: Map<string, Type>
    
    /// <summary>
    /// The name of the root type
    /// </summary>
    Root: string
}

/// <summary>
/// A position within a binary buffer
/// </summary>
[<Struct>]
type Position = {
    /// <summary>
    /// The byte offset
    /// </summary>
    Offset: int<offset>
    
    /// <summary>
    /// The line number (1-based)
    /// </summary>
    Line: int
    
    /// <summary>
    /// The column number (1-based)
    /// </summary>
    Column: int
}

/// <summary>
/// An address within a memory region
/// </summary>
/// <typeparam name="'region">The memory region measure type</typeparam>
[<Struct>]
type Address<[<Measure>] 'region> = {
    /// <summary>
    /// The absolute offset within the memory region
    /// </summary>
    Offset: int<offset>
}

/// <summary>
/// A type-safe pointer to native memory
/// </summary>
/// <typeparam name="'T">The type of data pointed to</typeparam>
/// <typeparam name="'region">The memory region the pointer refers to</typeparam>
/// <typeparam name="'access">The access pattern for the pointer (read/write/etc.)</typeparam>
[<Struct>]
type Ptr<'T, [<Measure>] 'region, [<Measure>] 'access> = {
    /// <summary>
    /// The native address
    /// </summary>
    Address: nativeint
}

/// <summary>
/// Access patterns for memory pointers
/// </summary>
module Access =
    /// <summary>
    /// Read-only access
    /// </summary>
    [<Measure>] type ReadOnly
    
    /// <summary>
    /// Read-write access
    /// </summary>
    [<Measure>] type ReadWrite
    
    /// <summary>
    /// Write-only access
    /// </summary>
    [<Measure>] type WriteOnly

/// <summary>
/// Contains functions for working with type sizes
/// </summary>
module Size =
    /// <summary>
    /// Creates a size representation for a fixed-size type
    /// </summary>
    /// <param name="size">The fixed size in bytes</param>
    /// <returns>A size with both Min and Max set to the same value</returns>
    let inline fixed' (size: int<bytes>): Size =
        { Min = size; Max = ValueOption.Some size; IsFixed = true }
    
    /// <summary>
    /// Creates a size representation for a variable-size type
    /// </summary>
    /// <param name="min">The minimum size in bytes</param>
    /// <param name="max">The optional maximum size in bytes</param>
    /// <returns>A size with the specified Min and Max</returns>
    let inline variable (min: int<bytes>) (max: ValueOption<int<bytes>>): Size =
        { Min = min; Max = max; IsFixed = false }
        
    /// <summary>
    /// Calculates size information from dimensions
    /// </summary>
    /// <param name="width">Width</param>
    /// <param name="height">Height</param>
    /// <param name="channels">Channels</param>
    /// <returns>Size in bytes</returns>
    let inline fromDimensions (width: int) (height: int) (channels: int): int<bytes> =
        let totalSize = width * height * channels
        intWithUnit<bytes> totalSize