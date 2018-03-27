namespace global  // note use of GLOBAL namespace

open System

//==============================================
// Helpers for Result type and AsyncResult type
//==============================================

// F# VERSION DIFFERENCE
// The "Result" type is built-in to F# 4.1 and newer (from VS2017),
// so uncomment the Result type if you are using an older version of F#

(*
/// The Result type represents a choice between success and failure
type Result<'success, 'failure> = 
    | Ok of 'success
    | Error of 'failure
*)

/// Functions for Result type (functor and monad).
/// For applicatives, see Validation.
[<RequireQualifiedAccess>]  // RequireQualifiedAccess forces the `Result.xxx` prefix to be used
module Result =

    /// Pass in a function to handle each case of `Result`
    let bimap onSuccess onError xR = 
        match xR with
        | Ok x -> onSuccess x
        | Error err -> onError err

        
    // F# VERSION DIFFERENCE
    // The `map`, `mapError` and `bind` functions are built-in to F# 4.1 and newer (from VS2017),
    // so uncomment these if you are using an older version of F#
    (*
    let map f result = 
        match result with
        | Ok success -> Ok (f success)
        | Error err -> Error err 

    let mapError f result = 
        match result with
        | Ok success -> Ok success
        | Error err -> Error (f err)

    let bind f result = 
        match result with
        | Ok success -> f success
        | Error err -> Error err
    *)

    // F# VERSION DIFFERENCE
    // The `map`, `mapError` and `bind` functions are in a different module in F# 4.1 and newer (from VS2017),
    // so these aliases make them available in this module.
    // In older versions of F#, where the functions are defined above, please comment them out
    let map = Result.map
    let mapError = Result.mapError
    let bind = Result.bind

    // Like `map` but with a unit-returning function
    let iter (f : _ -> unit) result = 
        map f result |> ignore    

    /// Apply a Result<fn> to a Result<x> monadically
    let apply fR xR = 
        match fR, xR with
        | Ok f, Ok x -> Ok (f x)
        | Error err1, Ok _ -> Error err1
        | Ok _, Error err2 -> Error err2
        | Error err1, Error _ -> Error err1 


    // combine a list of results, monadically
    let sequence aListOfResults = 
        let (<*>) = apply // monadic
        let (<!>) = map
        let cons head tail = head::tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = Ok [] // empty list inside Result
 
        // loop through the list, prepending each element
        // to the initial value
        List.foldBack consR aListOfResults initialValue



    //-----------------------------------
    // Lifting

    /// Lift a two parameter function to use Result parameters
    let lift2 f x1 x2 = 
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2

    /// Lift a three parameter function to use Result parameters
    let lift3 f x1 x2 x3 = 
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3

    /// Lift a four parameter function to use Result parameters
    let lift4 f x1 x2 x3 x4 = 
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3 <*> x4

    /// Apply a monadic function with two parameters 
    let bind2 f x1 x2 = lift2 f x1 x2 |> bind id

    /// Apply a monadic function with three parameters
    let bind3 f x1 x2 x3 = lift3 f x1 x2 x3 |> bind id

    //-----------------------------------
    // Predicates

    /// Predicate that returns true on success
    let isOk = 
        function 
        | Ok _ -> true
        | Error _ -> false

    /// Predicate that returns true on failure
    let isError xR = 
        xR |> isOk |> not

    /// Lift a given predicate into a predicate that works on Results
    let filter pred = 
        function 
        | Ok x -> pred x
        | Error _ -> true


    //-----------------------------------
    // Mixing simple values and results

    /// On success, return the value. On error, return a default value
    let ifError defaultVal = 
        function 
        | Ok x -> x
        | Error _ -> defaultVal


    //-----------------------------------
    // Mixing options and results

    /// Apply a monadic function to an Result<x option>
    let bindOption f xR =
        match xR with
        | Some x -> f x |> map Some
        | None -> Ok None

    /// Convert an Option into a Result. If none, use the passed-in errorValue 
    let ofOption errorValue opt = 
        match opt with
        | Some v -> Ok v
        | None -> Error errorValue

    /// Convert a Result into an Option 
    let toOption xR = 
        match xR with
        | Ok v -> Some v
        | Error _ -> None

    /// Convert the Error case into an Option 
    /// (useful with List.choose to find all errors in a list of Results)
    let toErrorOption = 
        function 
        | Ok _ -> None
        | Error err -> Some err


//==============================================
// Computation Expression for Result
//==============================================

[<AutoOpen>]
module ResultComputationExpression =

    type ResultBuilder() =
        member __.Return(x) = Ok x
        member __.Bind(x, f) = Result.bind f x
    
        member __.ReturnFrom(x) = x
        member this.Zero() = this.Return ()

        member __.Delay(f) = f
        member __.Run(f) = f()

        member this.While(guard, body) =
            if not (guard()) 
            then this.Zero() 
            else this.Bind( body(), fun () -> 
                this.While(guard, body))  

        member this.TryWith(body, handler) =
            try this.ReturnFrom(body())
            with e -> handler e

        member this.TryFinally(body, compensation) =
            try this.ReturnFrom(body())
            finally compensation() 

        member this.Using(disposable:#System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () -> 
                match disposable with 
                    | null -> () 
                    | disp -> disp.Dispose())

        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),fun enum -> 
                this.While(enum.MoveNext, 
                    this.Delay(fun () -> body enum.Current)))

        member this.Combine (a,b) = 
            this.Bind(a, fun () -> b())

    let result = new ResultBuilder()

//==============================================
// The `Validation` type is the same as the `Result` type but with a *list* for failures
// rather than a single value. This allows `Validation` types to be combined
// by combining their errors ("applicative-style")
//==============================================

type Validation<'Success,'Failure> = 
    Result<'Success,'Failure list>

/// Functions for the `Validation` type (mostly applicative)
[<RequireQualifiedAccess>]  // RequireQualifiedAccess forces the `Validation.xxx` prefix to be used
module Validation =

    /// Apply a Validation<fn> to a Validation<x> applicatively
    let apply (fV:Validation<_,_>) (xV:Validation<_,_>) :Validation<_,_> = 
        match fV, xV with
        | Ok f, Ok x -> Ok (f x)
        | Error errs1, Ok _ -> Error errs1
        | Ok _, Error errs2 -> Error errs2
        | Error errs1, Error errs2 -> Error (errs1 @ errs2)

    // combine a list of Validation, applicatively
    let sequence (aListOfValidations:Validation<_,_> list) = 
        let (<*>) = apply
        let (<!>) = Result.map
        let cons head tail = head::tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = Ok [] // empty list inside Result
  
        // loop through the list, prepending each element
        // to the initial value
        List.foldBack consR aListOfValidations initialValue

    //-----------------------------------
    // Converting between Validations and other types
    
    let ofResult xR :Validation<_,_> = 
        xR |> Result.mapError List.singleton

    let toResult (xV:Validation<_,_>) :Result<_,_> = 
        xV



//==============================================
// Async utilities
//==============================================

[<RequireQualifiedAccess>]  // RequireQualifiedAccess forces the `Async.xxx` prefix to be used
module Async =

    /// Lift a function to Async
    let map f xA = 
        async { 
        let! x = xA
        return f x 
        }

    /// Lift a value to Async
    let retn x = 
        async.Return x

    /// Apply an Async function to an Async value 
    let apply fA xA = 
        async { 
         // start the two asyncs in parallel
        let! fChild = Async.StartChild fA  // run in parallel
        let! x = xA
        // wait for the result of the first one
        let! f = fChild
        return f x 
        }

    /// Apply a monadic function to an Async value  
    let bind f xA = async.Bind(xA,f)


//==============================================
// AsyncResult
//==============================================

type AsyncResult<'Success,'Failure> = 
    Async<Result<'Success,'Failure>>

[<RequireQualifiedAccess>]  // RequireQualifiedAccess forces the `AsyncResult.xxx` prefix to be used
module AsyncResult =

    /// Lift a function to AsyncResult
    let map f (x:AsyncResult<_,_>) : AsyncResult<_,_> =
        Async.map (Result.map f) x

    /// Lift a function to AsyncResult
    let mapError f (x:AsyncResult<_,_>) : AsyncResult<_,_> =
        Async.map (Result.mapError f) x

    /// Apply ignore to the internal value
    let ignore x = 
        x |> map ignore    

    /// Lift a value to AsyncResult
    let retn x : AsyncResult<_,_> = 
        x |> Result.Ok |> Async.retn

    /// Handles asynchronous exceptions and maps them into Failure cases using the provided function
    let catch f (x:AsyncResult<_,_>) : AsyncResult<_,_> =
        x
        |> Async.Catch
        |> Async.map(function
            | Choice1Of2 (Ok v) -> Ok v
            | Choice1Of2 (Error err) -> Error err
            | Choice2Of2 ex -> Error (f ex))


    /// Apply an AsyncResult function to an AsyncResult value, monadically
    let applyM (fAsyncResult : AsyncResult<_, _>) (xAsyncResult : AsyncResult<_, _>) :AsyncResult<_,_> = 
        fAsyncResult |> Async.bind (fun fResult ->
        xAsyncResult |> Async.map (fun xResult -> Result.apply fResult xResult))

    /// Apply an AsyncResult function to an AsyncResult value, applicatively
    let applyA (fAsyncResult : AsyncResult<_, _>) (xAsyncResult : AsyncResult<_, _>) :AsyncResult<_,_> = 
        fAsyncResult |> Async.bind (fun fResult ->
        xAsyncResult |> Async.map (fun xResult -> Validation.apply fResult xResult))

    /// Apply a monadic function to an AsyncResult value  
    let bind (f: 'a -> AsyncResult<'b,'c>) (xAsyncResult : AsyncResult<_, _>) :AsyncResult<_,_> = async {
        let! xResult = xAsyncResult 
        match xResult with
        | Ok x -> return! f x
        | Error err -> return (Error err)
        }


    /// Convert a list of AsyncResult into a AsyncResult<list> using monadic style. 
    /// Only the first error is returned. The error type need not be a list.
    let sequenceM resultList = 
        let (<*>) = applyM
        let (<!>) = map
        let cons head tail = head::tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = retn [] // empty list inside Result
  
        // loop through the list, prepending each element
        // to the initial value
        List.foldBack consR resultList  initialValue


    /// Convert a list of AsyncResult into a AsyncResult<list> using applicative style. 
    /// All the errors are returned. The error type must be a list.
    let sequenceA resultList = 
        let (<*>) = applyA
        let (<!>) = map
        let cons head tail = head::tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = retn [] // empty list inside Result
  
        // loop through the list, prepending each element
        // to the initial value
        List.foldBack consR resultList  initialValue

    //-----------------------------------
    // Converting between AsyncResults and other types

    /// Lift a value into an Ok inside a AsyncResult
    let ofSuccess x : AsyncResult<_,_> = 
        x |> Result.Ok |> Async.retn 

    /// Lift a value into an Error inside a AsyncResult
    let ofError x : AsyncResult<_,_> = 
        x |> Result.Error |> Async.retn 

    /// Lift a Result into an AsyncResult
    let ofResult x : AsyncResult<_,_> = 
        x |> Async.retn

    /// Lift a Async into an AsyncResult
    let ofAsync x : AsyncResult<_,_> = 
        x |> Async.map Result.Ok

    //-----------------------------------
    // Utilities lifted from Async

    let sleep ms = 
        Async.Sleep ms |> ofAsync

    
// ==================================
// AsyncResult computation expression
// ==================================

/// The `asyncResult` computation expression is available globally without qualification
[<AutoOpen>]
module AsyncResultComputationExpression = 

    type AsyncResultBuilder() = 
        member __.Return(x) = AsyncResult.retn x
        member __.Bind(x, f) = AsyncResult.bind f x

        member __.ReturnFrom(x) = x
        member this.Zero() = this.Return ()

        member __.Delay(f) = f
        member __.Run(f) = f()

        member this.While(guard, body) =
            if not (guard()) 
            then this.Zero() 
            else this.Bind( body(), fun () -> 
                this.While(guard, body))  

        member this.TryWith(body, handler) =
            try this.ReturnFrom(body())
            with e -> handler e

        member this.TryFinally(body, compensation) =
            try this.ReturnFrom(body())
            finally compensation() 

        member this.Using(disposable:#System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () -> 
                match disposable with 
                    | null -> () 
                    | disp -> disp.Dispose())

        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),fun enum -> 
                this.While(enum.MoveNext, 
                    this.Delay(fun () -> body enum.Current)))

        member this.Combine (a,b) = 
            this.Bind(a, fun () -> b())

    let asyncResult = AsyncResultBuilder()