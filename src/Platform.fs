#nowarn "9"
namespace Alloy

/// Marker type indicating a function's implementation is provided by the platform.
/// Alex recognizes calls to functions in Platform.Bindings and generates
/// platform-specific code (syscalls, API calls, etc.).
[<Struct>]
type PlatformProvided = PlatformProvided

/// Platform-provided bindings for Fidelity native compilation.
///
/// NOTE: Core I/O (write/read/exit) are now provided by FNCS Sys intrinsics:
///   - Sys.write: fd:int -> buffer:nativeptr<byte> -> count:int -> int
///   - Sys.read:  fd:int -> buffer:nativeptr<byte> -> maxCount:int -> int
///   - Sys.exit:  code:int -> 'a
///
/// This module contains additional platform bindings (time, string, etc.) that
/// need Alex-provided implementations. These are stubs until the corresponding
/// FNCS intrinsics are implemented.
///
/// DO NOT add platform-specific code here. This module must remain platform-agnostic.
module Platform =

    open FSharp.NativeInterop

    /// Platform binding declarations for functions not yet provided as FNCS intrinsics.
    /// NOTE: Use Sys.write, Sys.read, Sys.exit directly for I/O and process control.
    module Bindings =

        // ═══════════════════════════════════════════════════════════════════════════
        // String Bindings (stub - needs FNCS intrinsic)
        // ═══════════════════════════════════════════════════════════════════════════

        /// Get length of a null-terminated string pointer.
        /// TODO: Should be implemented as native loop or FNCS intrinsic
        let strlen (str: nativeint) : int =
            Unchecked.defaultof<int>

        // ═══════════════════════════════════════════════════════════════════════════
        // Time Bindings (stubs - need FNCS intrinsics: Sys.clock_gettime, etc.)
        // ═══════════════════════════════════════════════════════════════════════════

        /// Get current time in ticks (100-nanosecond intervals since 0001-01-01).
        /// TODO: Needs Sys.clock_gettime intrinsic
        let getCurrentTicks () : int64 =
            Unchecked.defaultof<int64>

        /// Get high-resolution monotonic ticks for timing.
        /// TODO: Needs Sys.clock_gettime(MONOTONIC) intrinsic
        let getMonotonicTicks () : int64 =
            Unchecked.defaultof<int64>

        /// Get tick frequency (ticks per second) for high-resolution timer.
        /// TODO: Needs platform-specific frequency intrinsic
        let getTickFrequency () : int64 =
            Unchecked.defaultof<int64>

        /// Sleep for specified milliseconds.
        /// TODO: Needs Sys.nanosleep intrinsic
        let sleep (milliseconds: int) : unit =
            ()
