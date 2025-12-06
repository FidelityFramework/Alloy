#nowarn "9"
namespace Alloy

open Alloy.ValueOption

/// <summary>
/// Module containing Result operations for error handling and composition through 
/// statically resolved type parameters. All functions are exposed through the 
/// AutoOpen attribute, making them accessible when opening the Alloy namespace.
/// </summary>
[<AutoOpen>]
module Result =
    /// <summary>
    /// Applies a function to a Result value if it's Ok.
    /// </summary>
    /// <param name="f">The function to apply to the success value.</param>
    /// <param name="result">The result to transform.</param>
    /// <typeparam name="T">The type of the success value in the source Result.</typeparam>
    /// <typeparam name="U">The type of the success value in the target Result.</typeparam>
    /// <typeparam name="Error">The type of the error value.</typeparam>
    /// <returns>A new Result with the transformed value if Ok, otherwise the original Error.</returns>
    let map (f: 'T -> 'U) (result: Result<'T, 'Error>) : Result<'U, 'Error> =
        match result with
        | Ok value -> Ok(f value)
        | Error err -> Error err
    
    /// <summary>
    /// Applies a function to a Result error if it's Error.
    /// </summary>
    /// <param name="f">The function to apply to the error value.</param>
    /// <param name="result">The result to transform.</param>
    /// <typeparam name="T">The type of the success value.</typeparam>
    /// <typeparam name="Error1">The type of the error value in the source Result.</typeparam>
    /// <typeparam name="Error2">The type of the error value in the target Result.</typeparam>
    /// <returns>A new Result with the transformed error if Error, otherwise the original Ok.</returns>
    let mapError (f: 'Error1 -> 'Error2) (result: Result<'T, 'Error1>) : Result<'T, 'Error2> =
        match result with
        | Ok value -> Ok value
        | Error err -> Error(f err)
    
    /// <summary>
    /// Applies a binding function to a Result value if it's Ok.
    /// </summary>
    /// <param name="f">The function to apply to the success value, which itself returns a Result.</param>
    /// <param name="result">The result to transform.</param>
    /// <typeparam name="T">The type of the success value in the source Result.</typeparam>
    /// <typeparam name="U">The type of the success value in the target Result.</typeparam>
    /// <typeparam name="Error">The type of the error value.</typeparam>
    /// <returns>The result of applying the binding function if Ok, otherwise the original Error.</returns>
    let bind (f: 'T -> Result<'U, 'Error>) (result: Result<'T, 'Error>) : Result<'U, 'Error> =
        match result with
        | Ok value -> f value
        | Error err -> Error err
    
    /// <summary>
    /// Returns the result, or a default value if Error.
    /// </summary>
    /// <param name="defaultValue">The default value to use if the result is Error.</param>
    /// <param name="result">The result to check.</param>
    /// <typeparam name="T">The type of the success value.</typeparam>
    /// <typeparam name="Error">The type of the error value.</typeparam>
    /// <returns>The success value or the default value.</returns>
    let defaultValue (defaultValue: 'T) (result: Result<'T, 'Error>) : 'T =
        match result with
        | Ok value -> value
        | Error _ -> defaultValue
    
    /// <summary>
    /// Returns the result, or applies a function to the error value if Error.
    /// </summary>
    /// <param name="defaultFactory">The function to apply to the error value.</param>
    /// <param name="result">The result to check.</param>
    /// <typeparam name="T">The type of the success value.</typeparam>
    /// <typeparam name="Error">The type of the error value.</typeparam>
    /// <returns>The success value or the result of applying the function to the error.</returns>
    let defaultWith (defaultFactory: 'Error -> 'T) (result: Result<'T, 'Error>) : 'T =
        match result with
        | Ok value -> value
        | Error err -> defaultFactory err
    
    /// <summary>
    /// Creates an Ok result.
    /// </summary>
    /// <param name="value">The value to wrap.</param>
    /// <typeparam name="T">The type of the success value.</typeparam>
    /// <typeparam name="Error">The type of the error value.</typeparam>
    /// <returns>A Result containing the value as Ok.</returns>
    let ok (value: 'T) : Result<'T, 'Error> = Ok value
    
    /// <summary>
    /// Creates an Error result.
    /// </summary>
    /// <param name="error">The error to wrap.</param>
    /// <typeparam name="T">The type of the success value.</typeparam>
    /// <typeparam name="Error">The type of the error value.</typeparam>
    /// <returns>A Result containing the error as Error.</returns>
    let error (error: 'Error) : Result<'T, 'Error> = Error error
    
    /// <summary>
    /// Creates a Result from a ValueOption.
    /// </summary>
    /// <param name="error">The error value to use if the option has no value.</param>
    /// <param name="option">The ValueOption to convert.</param>
    /// <typeparam name="T">The type of the value.</typeparam>
    /// <typeparam name="Error">The type of the error value.</typeparam>
    /// <returns>Ok if Some, or Error with the provided error value if None.</returns>
    let ofValueOption (error: 'Error) (option: ValueOption<'T>) : Result<'T, 'Error> =
        if option.IsSome then 
            Ok option.Value
        else 
            Error error
    
    /// <summary>
    /// Creates a ValueOption from a Result.
    /// </summary>
    /// <param name="result">The Result to convert.</param>
    /// <typeparam name="T">The type of the success value.</typeparam>
    /// <typeparam name="Error">The type of the error value.</typeparam>
    /// <returns>Some if Ok, or None if Error.</returns>
    let toValueOption (result: Result<'T, 'Error>) : ValueOption<'T> =
        match result with
        | Ok value -> some value
        | Error _ -> none<'T>
    
    /// <summary>
    /// Creates a Result from an Option.
    /// </summary>
    /// <param name="error">The error value to use if the option has no value.</param>
    /// <param name="option">The Option to convert.</param>
    /// <typeparam name="T">The type of the value.</typeparam>
    /// <typeparam name="Error">The type of the error value.</typeparam>
    /// <returns>Ok if Some, or Error with the provided error value if None.</returns>
    let ofOption (error: 'Error) (option: ValueOption<'T>) : Result<'T, 'Error> =
        if option.IsSome then 
            Ok option.Value
        else 
            Error error
    
    /// <summary>
    /// Creates an Option from a Result.
    /// </summary>
    /// <param name="result">The Result to convert.</param>
    /// <typeparam name="T">The type of the success value.</typeparam>
    /// <typeparam name="Error">The type of the error value.</typeparam>
    /// <returns>Some if Ok, or None if Error.</returns>
    let toOption (result: Result<'T, 'Error>) : ValueOption<'T> =
        match result with
        | Ok value -> some value
        | Error _ -> none<'T>
    
    /// <summary>
    /// Returns true if the result is Ok.
    /// </summary>
    /// <param name="result">The result to check.</param>
    /// <typeparam name="T">The type of the success value.</typeparam>
    /// <typeparam name="Error">The type of the error value.</typeparam>
    /// <returns>True if the result is Ok, otherwise false.</returns>
    let isOk (result: Result<'T, 'Error>) : bool =
        match result with
        | Ok _ -> true
        | Error _ -> false
    
    /// <summary>
    /// Returns true if the result is Error.
    /// </summary>
    /// <param name="result">The result to check.</param>
    /// <typeparam name="T">The type of the success value.</typeparam>
    /// <typeparam name="Error">The type of the error value.</typeparam>
    /// <returns>True if the result is Error, otherwise false.</returns>
    let isError (result: Result<'T, 'Error>) : bool =
        match result with
        | Ok _ -> false
        | Error _ -> true
    
    /// <summary>
    /// Converts a Result<'T, 'Error> to a Result<'U, 'Error> by applying functions to both cases.
    /// </summary>
    /// <param name="mapFunc">The function to apply to the success value.</param>
    /// <param name="errorMapFunc">The function to apply to the error value.</param>
    /// <param name="result">The result to transform.</param>
    /// <typeparam name="T">The type of the success value in the source Result.</typeparam>
    /// <typeparam name="U">The type of the success value in the target Result.</typeparam>
    /// <typeparam name="Error1">The type of the error value in the source Result.</typeparam>
    /// <typeparam name="Error2">The type of the error value in the target Result.</typeparam>
    /// <returns>A new Result with transformed success and error values.</returns>
    let bimap (mapFunc: 'T -> 'U) (errorMapFunc: 'Error1 -> 'Error2) (result: Result<'T, 'Error1>) : Result<'U, 'Error2> =
        match result with
        | Ok value -> Ok(mapFunc value)
        | Error err -> Error(errorMapFunc err)
    
    /// <summary>
    /// Returns the result if Ok and the predicate returns true, otherwise returns the Error case.
    /// </summary>
    /// <param name="predicate">The predicate function to test the success value.</param>
    /// <param name="error">The error value to use if the predicate returns false.</param>
    /// <param name="result">The result to check.</param>
    /// <typeparam name="T">The type of the success value.</typeparam>
    /// <typeparam name="Error">The type of the error value.</typeparam>
    /// <returns>The original result if Ok and predicate returns true, otherwise Error.</returns>
    let filter (predicate: 'T -> bool) (error: 'Error) (result: Result<'T, 'Error>) : Result<'T, 'Error> =
        match result with
        | Ok value when predicate value -> Ok value
        | Ok _ -> Error error
        | Error err -> Error err
    
    /// <summary>
    /// Combines two results using a specified function, returns the first Error encountered if either is Error.
    /// </summary>
    /// <param name="mapping">The function to combine the two success values.</param>
    /// <param name="result1">The first result.</param>
    /// <param name="result2">The second result.</param>
    /// <typeparam name="T1">The type of the success value in the first Result.</typeparam>
    /// <typeparam name="T2">The type of the success value in the second Result.</typeparam>
    /// <typeparam name="U">The type of the success value in the target Result.</typeparam>
    /// <typeparam name="Error">The type of the error value.</typeparam>
    /// <returns>A new Result combining the success values if both are Ok, otherwise the first Error encountered.</returns>
    let map2 (mapping: 'T1 -> 'T2 -> 'U) (result1: Result<'T1, 'Error>) (result2: Result<'T2, 'Error>) : Result<'U, 'Error> =
        match result1, result2 with
        | Ok value1, Ok value2 -> Ok(mapping value1 value2)
        | Error err, _ -> Error err
        | _, Error err -> Error err