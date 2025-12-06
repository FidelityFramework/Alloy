#nowarn "9"
namespace Alloy.NativeTypes

open FSharp.NativeInterop

/// <summary>
/// Native string type for freestanding compilation.
/// This is a fat pointer (pointer + length) that Firefly emits as an MLIR struct.
/// Similar to Rust's &str or Fable's LrcStr for the Rust backend.
/// </summary>
[<AutoOpen>]
module NativeString =

    /// <summary>
    /// A native string is a pointer to UTF-8 bytes plus a length.
    /// This struct is emitted by Firefly as: !llvm.struct<(ptr, i64)>
    /// </summary>
    [<Struct>]
    [<NoEquality; NoComparison>]
    type NativeStr =
        val Pointer: nativeptr<byte>
        val Length: int

        new (ptr: nativeptr<byte>, len: int) =
            { Pointer = ptr; Length = len }

    /// <summary>
    /// Creates a NativeStr from a pointer and length.
    /// This is the primary constructor used by Console.ReadLine and similar operations.
    /// </summary>
    let inline create (ptr: nativeptr<byte>) (len: int) : NativeStr =
        NativeStr(ptr, len)

    /// <summary>
    /// Creates an empty NativeStr.
    /// </summary>
    let inline empty : NativeStr =
        NativeStr(NativePtr.nullPtr<byte>, 0)

    /// <summary>
    /// Returns true if the string is empty.
    /// </summary>
    let inline isEmpty (s: NativeStr) : bool =
        s.Length = 0

    /// <summary>
    /// Returns the length of the string in bytes.
    /// </summary>
    let inline length (s: NativeStr) : int =
        s.Length

    /// <summary>
    /// Gets the byte at the specified index.
    /// </summary>
    let inline byteAt (index: int) (s: NativeStr) : byte =
        if index < 0 || index >= s.Length then
            0uy
        else
            NativePtr.get s.Pointer index

    /// <summary>
    /// Writes the contents of a NativeStr to a destination buffer.
    /// Returns the number of bytes written.
    /// </summary>
    let inline copyTo (dest: nativeptr<byte>) (s: NativeStr) : int =
        for i = 0 to s.Length - 1 do
            NativePtr.set dest i (NativePtr.get s.Pointer i)
        s.Length
