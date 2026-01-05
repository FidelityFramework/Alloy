#nowarn "9"
namespace Alloy

// Time.fs uses FNCS Sys intrinsics for time operations:
// - Sys.clock_gettime : unit -> int64  (wall clock ticks)
// - Sys.clock_monotonic : unit -> int64  (monotonic ticks)
// - Sys.tick_frequency : unit -> int64  (ticks per second)
// - Sys.nanosleep : int -> unit  (sleep for milliseconds)

/// BCL-compatible DateTime type for Fidelity native compilation.
/// Uses Sys.clock_gettime() FNCS intrinsic for time retrieval.
[<Struct>]
type DateTime = {
    /// Internal ticks representation (100-nanosecond intervals since 0001-01-01)
    Ticks: int64
}
with
    /// Gets the year component
    member this.Year: int =
        // Simplified calculation - proper calendar math would be more complex
        int ((this.Ticks / 315360000000000L) + 1L)

    /// Gets the month component (1-12)
    member this.Month: int = 1  // TODO: Proper calendar calculation

    /// Gets the day component (1-31)
    member this.Day: int = 1  // TODO: Proper calendar calculation

    /// Gets the hour component (0-23)
    member this.Hour: int =
        int ((this.Ticks / 36000000000L) % 24L)

    /// Gets the minute component (0-59)
    member this.Minute: int =
        int ((this.Ticks / 600000000L) % 60L)

    /// Gets the second component (0-59)
    member this.Second: int =
        int ((this.Ticks / 10000000L) % 60L)

    /// Gets the millisecond component (0-999)
    member this.Millisecond: int =
        int ((this.Ticks / 10000L) % 1000L)

    /// Returns a string representation of the DateTime
    member this.ToString() : string =
        $"{this.Year}-{this.Month}-{this.Day} {this.Hour}:{this.Minute}:{this.Second}"

/// BCL-compatible DateTime static members.
/// Uses Sys.clock_gettime() FNCS intrinsic for time.
module DateTime =
    /// Gets the current local date and time.
    /// Uses Sys.clock_gettime() FNCS intrinsic (Alex emits platform syscall).
    let inline Now () : DateTime = { Ticks = Sys.clock_gettime() }

    /// Gets the current UTC date and time.
    /// For now, same as Now (timezone handling would require additional bindings).
    let inline UtcNow () : DateTime = { Ticks = Sys.clock_gettime() }

    /// Gets today's date with time set to 00:00:00.
    let inline Today () : DateTime =
        let ticks = Sys.clock_gettime()
        let ticksPerDay = 864000000000L
        { Ticks = (ticks / ticksPerDay) * ticksPerDay }

/// BCL-compatible TimeSpan type
[<Struct>]
type TimeSpan = {
    /// Total ticks (100-nanosecond intervals)
    Ticks: int64
}
with
    /// Gets the days component
    member this.Days: int = int (this.Ticks / 864000000000L)

    /// Gets the hours component
    member this.Hours: int = int ((this.Ticks / 36000000000L) % 24L)

    /// Gets the minutes component
    member this.Minutes: int = int ((this.Ticks / 600000000L) % 60L)

    /// Gets the seconds component
    member this.Seconds: int = int ((this.Ticks / 10000000L) % 60L)

    /// Gets the milliseconds component
    member this.Milliseconds: int = int ((this.Ticks / 10000L) % 1000L)

    /// Gets the total milliseconds
    member this.TotalMilliseconds: float = float this.Ticks / 10000.0

/// BCL-compatible Thread.Sleep.
/// Uses Sys.nanosleep() FNCS intrinsic.
module Threading =
    module Thread =
        /// Suspends the current thread for the specified number of milliseconds.
        /// Uses Sys.nanosleep() FNCS intrinsic (Alex emits platform syscall).
        let inline Sleep (milliseconds: int) : unit =
            Sys.nanosleep milliseconds

/// Alloy-specific time utilities (non-BCL).
/// These provide lower-level access for performance-critical code.
/// All operations use FNCS Sys intrinsics.
[<RequireQualifiedAccess>]
module Time =
    /// Gets the current time in ticks (100-nanosecond intervals since 0001-01-01).
    /// Uses Sys.clock_gettime() FNCS intrinsic.
    let inline currentTicks () : int64 =
        Sys.clock_gettime()

    /// Gets high-resolution monotonic ticks for timing.
    /// Uses Sys.clock_monotonic() FNCS intrinsic.
    let inline highResolutionTicks () : int64 =
        Sys.clock_monotonic()

    /// Gets the frequency of the high-resolution timer (ticks per second).
    /// Uses Sys.tick_frequency() FNCS intrinsic.
    let inline tickFrequency () : int64 =
        Sys.tick_frequency()

    /// Gets the current Unix timestamp (seconds since 1970-01-01).
    /// Computed from ticks with epoch conversion.
    let inline currentUnixTimestamp () : int64 =
        let ticks = Sys.clock_gettime()
        // Unix epoch is 1970-01-01, .NET epoch is 0001-01-01
        // Difference: 621355968000000000 ticks
        let unixEpochTicks = 621355968000000000L
        let ticksPerSecond = 10000000L
        (ticks - unixEpochTicks) / ticksPerSecond

    /// Sleeps for the specified number of milliseconds.
    /// Uses Sys.nanosleep() FNCS intrinsic.
    let inline sleep (milliseconds: int) : unit =
        Sys.nanosleep milliseconds
