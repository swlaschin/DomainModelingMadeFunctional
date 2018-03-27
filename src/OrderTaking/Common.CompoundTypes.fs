// We are defining types and submodules, so we can use a namespace
// rather than a module at the top level
namespace OrderTaking.Common  

open System

// ==================================
// Common compound types used throughout the OrderTaking domain
//
// Includes: customers, addresses, etc.
// Plus common errors.
//
// ==================================


// ==================================
// Customer-related types
// ==================================

type PersonalName = {
    FirstName : String50
    LastName : String50
    }
    
type CustomerInfo = {
    Name : PersonalName 
    EmailAddress : EmailAddress 
    }

// ==================================
// Address-related
// ==================================

type Address = {
    AddressLine1 : String50
    AddressLine2 : String50 option
    AddressLine3 : String50 option
    AddressLine4 : String50 option
    City : String50
    ZipCode : ZipCode
    }

// ==================================
// Product-related types
// ==================================

// Note that the definition of a Product is in a different bounded
// context, and in this context, products are only represented by a ProductCode
// (see the SimpleTypes module).


