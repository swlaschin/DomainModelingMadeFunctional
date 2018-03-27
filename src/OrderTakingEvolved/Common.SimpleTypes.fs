// We are defining types and submodules, so we can use a namespace
// rather than a module at the top level
namespace OrderTaking.Common  

open System

// ===============================
// Simple types and constrained types related to the OrderTaking domain.
//
// E.g. Single case discriminated unions (aka wrappers), enums, etc
// ===============================

/// Constrained to be 50 chars or less, not null
type String50 = private String50 of string

/// An email address
type EmailAddress = private EmailAddress of string

/// Customer's VIP status
type VipStatus =
    | Normal 
    | Vip 

/// A zip code
type ZipCode = private ZipCode of string

/// A US 2 letter state code
type UsStateCode = private UsStateCode of string


/// An Id for Orders. Constrained to be a non-empty string < 10 chars
type OrderId = private OrderId of string

/// An Id for OrderLines. Constrained to be a non-empty string < 10 chars
type OrderLineId = private OrderLineId of string

/// The codes for Widgets start with a "W" and then four digits
type WidgetCode = private WidgetCode of string  

/// The codes for Gizmos start with a "G" and then three digits. 
type GizmoCode = private GizmoCode of string  

/// A ProductCode is either a Widget or a Gizmo
type ProductCode =
    | Widget of WidgetCode
    | Gizmo of GizmoCode

/// Constrained to be a integer between 1 and 1000
type UnitQuantity = private UnitQuantity of int 

/// Constrained to be a decimal between 0.05 and 100.00 
type KilogramQuantity = private KilogramQuantity of decimal

/// A Quantity is either a Unit or a Kilogram
type OrderQuantity =
    | Unit of UnitQuantity
    | Kilogram of KilogramQuantity

/// Constrained to be a decimal between 0.0 and 1000.00 
type Price = private Price of decimal

/// Constrained to be a decimal between 0.0 and 10000.00 
type BillingAmount = private BillingAmount of decimal

/// Represents a PDF attachment
type PdfAttachment = {
    Name : string
    Bytes: byte[]
    }

type PromotionCode = PromotionCode of string


// ===============================
// Reusable constructors and getters for constrained types
// ===============================

/// Useful functions for constrained types
module ConstrainedType =

    /// Create a constrained string using the constructor provided
    /// Return Error if input is null, empty, or length > maxLen
    let createString fieldName ctor maxLen str = 
        if String.IsNullOrEmpty(str) then
            let msg = sprintf "%s must not be null or empty" fieldName 
            Error msg
        elif str.Length > maxLen then
            let msg = sprintf "%s must not be more than %i chars" fieldName maxLen 
            Error msg 
        else
            Ok (ctor str)

    /// Create a optional constrained string using the constructor provided
    /// Return None if input is null, empty. 
    /// Return error if length > maxLen
    /// Return Some if the input is valid
    let createStringOption fieldName ctor maxLen str = 
        if String.IsNullOrEmpty(str) then
            Ok None
        elif str.Length > maxLen then
            let msg = sprintf "%s must not be more than %i chars" fieldName maxLen 
            Error msg 
        else
            Ok (ctor str |> Some)

    /// Create a constrained integer using the constructor provided
    /// Return Error if input is less than minVal or more than maxVal
    let createInt fieldName ctor minVal maxVal i = 
        if i < minVal then
            let msg = sprintf "%s: Must not be less than %i" fieldName minVal
            Error msg
        elif i > maxVal then
            let msg = sprintf "%s: Must not be greater than %i" fieldName maxVal
            Error msg
        else
            Ok (ctor i)

    /// Create a constrained decimal using the constructor provided
    /// Return Error if input is less than minVal or more than maxVal
    let createDecimal fieldName ctor minVal maxVal i = 
        if i < minVal then
            let msg = sprintf "%s: Must not be less than %M" fieldName minVal
            Error msg
        elif i > maxVal then
            let msg = sprintf "%s: Must not be greater than %M" fieldName maxVal
            Error msg
        else
            Ok (ctor i)

    /// Create a constrained string using the constructor provided
    /// Return Error if input is null. empty, or does not match the regex pattern
    let createLike fieldName  ctor pattern str = 
        if String.IsNullOrEmpty(str) then
            let msg = sprintf "%s: Must not be null or empty" fieldName 
            Error msg
        elif System.Text.RegularExpressions.Regex.IsMatch(str,pattern) then
            Ok (ctor str)
        else
            let msg = sprintf "%s: '%s' must match the pattern '%s'" fieldName str pattern
            Error msg 


// F# VERSION DIFFERENCE
// Modules with the same name as a non-generic type will cause an error in versions of F# before v4.1 (VS2017) 
// so change the module definition to include a [<CompilationRepresentation>] attribute like this:
(*
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String50 =
*)

module String50 =

    /// Return the value inside a String50
    let value (String50 str) = str

    /// Create an String50 from a string
    /// Return Error if input is null, empty, or length > 50
    let create fieldName str = 
        ConstrainedType.createString fieldName String50 50 str

    /// Create an String50 from a string
    /// Return None if input is null, empty. 
    /// Return error if length > maxLen
    /// Return Some if the input is valid
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName String50 50 str

module EmailAddress =

    /// Return the string value inside an EmailAddress 
    let value (EmailAddress str) = str

    /// Create an EmailAddress from a string
    /// Return Error if input is null, empty, or doesn't have an "@" in it
    let create fieldName str = 
        let pattern = ".+@.+" // anything separated by an "@"
        ConstrainedType.createLike fieldName EmailAddress pattern str

module VipStatus =

    /// Return a string representation of VipStatus 
    let value status = 
        match status with
        | Normal -> "Normal"
        | Vip -> "VIP"

    /// Create a VipStatus from a string
    /// Return Error if input is null, empty, or doesn't match one of the cases
    let create fieldName str = 
        match str with
        | "normal" | "Normal" -> 
            Ok Normal
        | "vip" | "VIP" -> 
            Ok Vip
        | _ -> 
            // all other cases
            let msg = sprintf "%s: Must be one of 'Normal', 'VIP'" fieldName 
            Error msg

module ZipCode =

    /// Return the string value inside a ZipCode
    let value (ZipCode str) = str

    /// Create a ZipCode from a string
    /// Return Error if input is null, empty, or doesn't have 5 digits
    let create fieldName str = 
        let pattern = "\d{5}"
        ConstrainedType.createLike fieldName ZipCode pattern str


module UsStateCode =

    /// Return the string value inside a UsStateCode 
    let value (UsStateCode str) = str

    /// Create a UsStateCode from a string
    /// Return Error if input is null, empty, or doesn't have 2 letters
    let create fieldName str = 
        let pattern = "^(A[KLRZ]|C[AOT]|D[CE]|FL|GA|HI|I[ADLN]|K[SY]|LA|M[ADEINOST]|N[CDEHJMVY]|O[HKR]|P[AR]|RI|S[CD]|T[NX]|UT|V[AIT]|W[AIVY])$"
        ConstrainedType.createLike fieldName UsStateCode pattern str


module OrderId =

    /// Return the string value inside an OrderId
    let value (OrderId str) = str

    /// Create an OrderId from a string
    /// Return Error if input is null, empty, or length > 50
    let create fieldName str = 
        ConstrainedType.createString fieldName OrderId 50 str

module OrderLineId =

    /// Return the string value inside an OrderLineId 
    let value (OrderLineId str) = str

    /// Create an OrderLineId from a string
    /// Return Error if input is null, empty, or length > 50
    let create fieldName str = 
        ConstrainedType.createString fieldName OrderLineId 50 str

module WidgetCode =

    /// Return the string value inside a WidgetCode 
    let value (WidgetCode code) = code

    /// Create an WidgetCode from a string
    /// Return Error if input is null. empty, or not matching pattern
    let create fieldName code = 
        // The codes for Widgets start with a "W" and then four digits
        let pattern = "W\d{4}"
        ConstrainedType.createLike fieldName WidgetCode pattern code 

module GizmoCode =

    /// Return the string value inside a GizmoCode
    let value (GizmoCode code) = code

    /// Create an GizmoCode from a string
    /// Return Error if input is null, empty, or not matching pattern
    let create fieldName code = 
        // The codes for Gizmos start with a "G" and then three digits. 
        let pattern = "G\d{3}"
        ConstrainedType.createLike fieldName GizmoCode pattern code 

module ProductCode =

    /// Return the string value inside a ProductCode 
    let value productCode = 
        match productCode with
        | Widget (WidgetCode wc) -> wc
        | Gizmo (GizmoCode gc) -> gc

    /// Create an ProductCode from a string
    /// Return Error if input is null, empty, or not matching pattern
    let create fieldName code = 
        if String.IsNullOrEmpty(code) then
            let msg = sprintf "%s: Must not be null or empty" fieldName 
            Error msg
        else if code.StartsWith("W") then
            WidgetCode.create fieldName code 
            |> Result.map Widget
        else if code.StartsWith("G") then
            GizmoCode.create fieldName code
            |> Result.map Gizmo
        else 
            let msg = sprintf "%s: Format not recognized '%s'" fieldName code
            Error msg


module UnitQuantity  =

    /// Return the value inside a UnitQuantity 
    let value (UnitQuantity v) = v

    /// Create a UnitQuantity from a int
    /// Return Error if input is not an integer between 1 and 1000 
    let create fieldName v = 
        ConstrainedType.createInt fieldName UnitQuantity 1 1000 v

module KilogramQuantity =

    /// Return the value inside a KilogramQuantity 
    let value (KilogramQuantity v) = v

    /// Create a KilogramQuantity from a decimal.
    /// Return Error if input is not a decimal between 0.05 and 100.00 
    let create fieldName v = 
        ConstrainedType.createDecimal fieldName KilogramQuantity 0.5M 100M v

module OrderQuantity  =

    /// Return the value inside a OrderQuantity  
    let value qty = 
        match qty with
        | Unit uq -> 
            uq |> UnitQuantity.value |> decimal
        | Kilogram kq -> 
            kq |> KilogramQuantity.value 

    /// Create a OrderQuantity from a productCode and quantity  
    let create fieldName productCode quantity  = 
        match productCode with
        | Widget _ -> 
            UnitQuantity.create fieldName (int quantity) // convert float to int 
            |> Result.map OrderQuantity.Unit             // lift to OrderQuantity type
        | Gizmo _ -> 
            KilogramQuantity.create fieldName quantity 
            |> Result.map OrderQuantity.Kilogram         // lift to OrderQuantity type

module Price =

    /// Return the value inside a Price 
    let value (Price v) = v

    /// Create a Price from a decimal.
    /// Return Error if input is not a decimal between 0.0 and 1000.00 
    let create v = 
        ConstrainedType.createDecimal "Price" Price 0.0M 1000M v

    /// Create a Price from a decimal.
    /// Throw an exception if out of bounds. This should only be used if you know the value is valid.
    let unsafeCreate v = 
        create v 
        |> function
            | Ok price -> 
                price
            | Error err -> 
                failwithf "Not expecting Price to be out of bounds: %s" err

    /// Multiply a Price by a decimal qty.
    /// Return Error if new price is out of bounds.
    let multiply qty (Price p) = 
        create (qty * p)

module BillingAmount =

    /// Return the value inside a BillingAmount
    let value (BillingAmount v) = v

    /// Create a BillingAmount from a decimal.
    /// Return Error if input is not a decimal between 0.0 and 10000.00 
    let create v = 
        ConstrainedType.createDecimal "BillingAmount" BillingAmount 0.0M 10000M v

    /// Sum a list of prices to make a billing amount
    /// Return Error if total is out of bounds
    let sumPrices prices =
        let total = prices |> List.map Price.value |> List.sum
        create total 
