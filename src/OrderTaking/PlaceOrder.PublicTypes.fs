// We are defining types and submodules, so we can use a namespace
// rather than a module at the top level
namespace OrderTaking.PlaceOrder

open OrderTaking.Common

// ==================================
// This file contains the definitions of PUBLIC types (exposed at the boundary of the bounded context)
// related to the PlaceOrder workflow 
// ==================================

// ------------------------------------
// inputs to the workflow

type UnvalidatedCustomerInfo = {
    FirstName : string
    LastName : string
    EmailAddress : string
    }

type UnvalidatedAddress = {
    AddressLine1 : string
    AddressLine2 : string
    AddressLine3 : string
    AddressLine4 : string
    City : string
    ZipCode : string
    }

type UnvalidatedOrderLine =  {
    OrderLineId : string
    ProductCode : string
    Quantity : decimal
    }

type UnvalidatedOrder = {
    OrderId : string
    CustomerInfo : UnvalidatedCustomerInfo
    ShippingAddress : UnvalidatedAddress
    BillingAddress : UnvalidatedAddress
    Lines : UnvalidatedOrderLine list
    }


// ------------------------------------
// outputs from the workflow (success case)

/// Event will be created if the Acknowledgment was successfully posted
type OrderAcknowledgmentSent = {
    OrderId : OrderId
    EmailAddress : EmailAddress 
    }


// priced state            
type PricedOrderLine = {
    OrderLineId : OrderLineId 
    ProductCode : ProductCode 
    Quantity : OrderQuantity
    LinePrice : Price
    }

type PricedOrder = {
    OrderId : OrderId
    CustomerInfo : CustomerInfo
    ShippingAddress : Address
    BillingAddress : Address
    AmountToBill : BillingAmount
    Lines : PricedOrderLine list
    }

/// Event to send to shipping context
type OrderPlaced = PricedOrder

/// Event to send to billing context
/// Will only be created if the AmountToBill is not zero
type BillableOrderPlaced = {
    OrderId : OrderId
    BillingAddress: Address
    AmountToBill : BillingAmount
    }

/// The possible events resulting from the PlaceOrder workflow
/// Not all events will occur, depending on the logic of the workflow
type PlaceOrderEvent = 
    | OrderPlaced of OrderPlaced
    | BillableOrderPlaced of BillableOrderPlaced 
    | AcknowledgmentSent  of OrderAcknowledgmentSent



// ------------------------------------
// error outputs 


/// All the things that can go wrong in this workflow
type ValidationError = ValidationError of string

type PricingError = PricingError of string

type ServiceInfo = {
    Name : string
    Endpoint: System.Uri
    }

type RemoteServiceError = {
    Service : ServiceInfo 
    Exception : System.Exception
    }

type PlaceOrderError =
    | Validation of ValidationError 
    | Pricing of PricingError 
    | RemoteService of RemoteServiceError 


// ------------------------------------
// the workflow itself

type PlaceOrder = 
    UnvalidatedOrder -> AsyncResult<PlaceOrderEvent list,PlaceOrderError>



