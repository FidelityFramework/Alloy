#nowarn "9"
namespace Alloy

open FSharp.NativeInterop

/// Primitive operations and bindings that must be defined before all other Alloy modules.
/// Alex intercepts calls to Primitives.Bindings and provides platform-specific implementations.
///
/// NOTE: String type with native semantics (UTF-8 fat pointer) is provided by FNCS.
/// In native compilation, 'string' has members: Pointer (nativeptr<byte>), Length (int)
[<AutoOpen>]
module Primitives =

    /// Standard file descriptors
    [<Literal>]
    let STDIN = 0
    [<Literal>]
    let STDOUT = 1
    [<Literal>]
    let STDERR = 2

    /// Creates a string from a byte literal.
    /// In native compilation, FNCS handles byte literal → string conversion intrinsically.
    /// The -1 accounts for F#'s null terminator in byte literals.
    ///
    /// NATIVE: FNCS provides this as an intrinsic - byte literals become NativeStr directly.
    /// This function exists for API consistency; actual implementation is compiler-provided.
    let inline ofBytes (bytes: byte[]) : string =
        // FNCS intrinsic: byte[] literal → NativeStr
        // The compiler recognizes this pattern and emits the string directly
        // Length is bytes.Length - 1 (exclude null terminator)
        Unchecked.defaultof<string> // Placeholder - FNCS provides actual implementation

    /// Creates a string from a buffer pointer and length.
    /// In native compilation, FNCS provides this as an intrinsic.
    ///
    /// NATIVE: Constructs NativeStr = {ptr: pointer, len: length}
    /// This function exists for API consistency; actual implementation is compiler-provided.
    let inline fromPointer (pointer: nativeptr<byte>) (length: int) : string =
        // FNCS intrinsic: (ptr, len) → NativeStr
        // The compiler recognizes this pattern and constructs the string directly
        Unchecked.defaultof<string> // Placeholder - FNCS provides actual implementation

    /// Write a string to a file descriptor using Sys.write intrinsic.
    /// In native compilation, string is a UTF-8 fat pointer with intrinsic Pointer and Length.
    ///
    /// NATIVE: NativeStr = {ptr: *u8, len: usize}
    /// FNCS provides: s.Pointer (nativeptr<byte>), s.Length (int)
    let inline writeStr (fd: int) (s: string) : int =
        // Native string IS UTF-8 bytes - no encoding needed
        // Access intrinsic members provided by FNCS for NativeStr type
        // Sys.write is an FNCS intrinsic: fd:int -> buffer:nativeptr<byte> -> count:int -> int
        Sys.write fd s.Pointer s.Length

    /// Write a string to stderr.
    let inline writeErr (s: string) : unit =
        writeStr STDERR s |> ignore

    /// Write a newline to a file descriptor.
    let inline writeNewline (fd: int) : unit =
        let mutable nl = 10uy
        Sys.write fd &&nl 1 |> ignore

    /// Native panic that aborts the program with exit code 1.
    /// Writes the error message to stderr before aborting.
    /// Usage: panicwith "message" or panicwith (ofBytes "message"B)
    let inline panicwith (message: string) : 'T =
        // Write "panic: " prefix
        writeErr "panic: "
        // Write the error message
        writeErr message
        // Write newline
        writeNewline STDERR
        // Abort with exit code 1
        // Sys.exit has type int -> 'a (never returns, polymorphic return)
        Sys.exit 1
