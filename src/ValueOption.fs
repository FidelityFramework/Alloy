#nowarn "9"
namespace Alloy

/// <summary>
/// ValueOption wraps F#'s built-in voption for zero-cost value-type options.
/// This avoids heap allocations by using the struct-based voption type.
/// </summary>
/// <typeparam name="T">The type of the optional value.</typeparam>
type ValueOption<'T> = voption<'T>

/// <summary>
/// Provides idiomatic Some/None patterns that transparently use voption.
/// This allows natural F# syntax while guaranteeing SRTP-safe value types.
/// </summary>
[<AutoOpen>]
module ValueOptionPatterns =
    /// <summary>Active pattern for matching ValueOption with idiomatic Some/None syntax.</summary>
    let (|Some|None|) (opt: ValueOption<'T>) =
        match opt with
        | ValueSome v -> Some v
        | ValueNone -> None

/// <summary>
/// Provides idiomatic Some/None constructors that create voption values.
/// </summary>
[<AutoOpen>]
module ValueOptionConstructors =
    /// <summary>Creates a ValueOption containing the value (actually ValueSome).</summary>
    let Some value : ValueOption<'T> = ValueSome value

    /// <summary>Returns an empty ValueOption (actually ValueNone).</summary>
    let None<'T> : ValueOption<'T> = ValueNone

/// <summary>
/// Functions for working with the ValueOption&lt;'T&gt; type (alias for voption).
/// </summary>
module ValueOption =
    /// <summary>Creates a Some value.</summary>
    let inline some v : ValueOption<'T> = Some v

    /// <summary>Returns the None value.</summary>
    let inline none<'T> : ValueOption<'T> = None

    /// <summary>Checks if an option has a value.</summary>
    let inline isSome (opt: ValueOption<'T>) = opt.IsSome

    /// <summary>Checks if an option has no value.</summary>
    let inline isNone (opt: ValueOption<'T>) = opt.IsNone

    /// <summary>Gets the value of a Some option, throws if None.</summary>
    let inline value (opt: ValueOption<'T>) = opt.Value

    /// <summary>Returns the value if Some, or the default value if None.</summary>
    let inline defaultValue defVal (opt: ValueOption<'T>) =
        match opt with
        | Some v -> v
        | None -> defVal

    /// <summary>Returns the value if Some, or calls the generator function if None.</summary>
    let inline defaultWith generator (opt: ValueOption<'T>) =
        match opt with
        | Some v -> v
        | None -> generator()

    /// <summary>Transforms the value inside an option.</summary>
    let inline map mapping (opt: ValueOption<'T>) : ValueOption<'U> =
        match opt with
        | Some v -> Some (mapping v)
        | None -> None

    /// <summary>Transforms an option with a function that returns an option.</summary>
    let inline bind binder (opt: ValueOption<'T>) : ValueOption<'U> =
        match opt with
        | Some v -> binder v
        | None -> None

    /// <summary>Checks equality between two ValueOptions.</summary>
    let inline equals (a: ValueOption<'T>) (b: ValueOption<'T>) =
        match a, b with
        | None, None -> true
        | Some va, Some vb -> va = vb
        | _ -> false

    /// <summary>Converts a ValueOption to a standard F# option.</summary>
    let inline toOption (opt: voption<'T>) =
        match opt with
        | ValueSome v -> FSharp.Core.Some v
        | ValueNone -> FSharp.Core.None