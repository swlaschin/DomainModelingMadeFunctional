namespace OrderTaking.PlaceOrder

open System.Collections.Generic

open OrderTaking
open OrderTaking.Common

// ======================================================
// This file contains the logic for working with data transfer objects (DTOs)
//
// This represents the code in chapter 11, "Serialization"
//
// Each type of DTO is defined using primitive, serializable types
// and then there are `toDomain` and `fromDomain` functions defined for each DTO.
//
// ======================================================


// ==================================
// DTOs for PlaceOrder workflow
// ==================================

[<AutoOpen>]
module internal Utils =     

    /// Helper function to get the value from an Option, and if None, use the defaultValue 
    /// Note that the defaultValue is the first parameter, unlike the similar `defaultArg`
    let defaultIfNone defaultValue opt =
        match opt with
        | Some v -> v
        | None -> defaultValue
        // could also use
        // defaultArg opt defaultValue

//===============================================
// DTO for CustomerInfo
//===============================================

type CustomerInfoDto = {
    FirstName : string
    LastName : string
    EmailAddress : string
    }

/// Functions for converting between the DTO and corresponding domain object
module internal CustomerInfoDto =
        
    /// Convert the DTO into a UnvalidatedCustomerInfo object.
    /// This always succeeds because there is no validation. 
    /// Used when importing an OrderForm from the outside world into the domain.
    let toUnvalidatedCustomerInfo (dto:CustomerInfoDto) : UnvalidatedCustomerInfo =
            
        // sometimes it's helpful to use an explicit type annotation 
        // to avoid ambiguity between records with the same field names.
        let domainObj : UnvalidatedCustomerInfo = {  

            // this is a simple 1:1 copy which always succeeds
            FirstName = dto.FirstName 
            LastName = dto.LastName 
            EmailAddress = dto.EmailAddress
            } 
        domainObj 

    /// Convert the DTO into a CustomerInfo object
    /// Used when importing from the outside world into the domain, eg loading from a database
    let toCustomerInfo (dto:CustomerInfoDto) :Result<CustomerInfo,string> =
        result {
            // get each (validated) simple type from the DTO as a success or failure
            let! first = dto.FirstName |> String50.create "FirstName"
            let! last = dto.LastName |> String50.create "LastName"
            let! email = dto.EmailAddress|> EmailAddress.create "EmailAddress"
            // combine the components to create the domain object
            let name = {FirstName = first; LastName = last}
            let info = {Name = name; EmailAddress = email}
            return info
            }

    /// Convert a CustomerInfo object into the corresponding DTO.
    /// Used when exporting from the domain to the outside world.
    let fromCustomerInfo (domainObj:Common.CustomerInfo) :CustomerInfoDto = 
        // this is a simple 1:1 copy
        {
        FirstName = domainObj.Name.FirstName |> String50.value
        LastName = domainObj.Name.LastName |> String50.value
        EmailAddress = domainObj.EmailAddress |> EmailAddress.value
        }

//===============================================
// DTO for Address 
//===============================================
        
type AddressDto = {
    AddressLine1 : string
    AddressLine2 : string
    AddressLine3 : string
    AddressLine4 : string
    City : string
    ZipCode : string
    }

/// Functions for converting between the DTO and corresponding domain object
module internal AddressDto =

    /// Convert the DTO into a UnvalidatedAddress 
    /// This always succeeds because there is no validation. 
    /// Used when importing an OrderForm from the outside world into the domain.
    let toUnvalidatedAddress (dto:AddressDto) :UnvalidatedAddress = 
        // this is a simple 1:1 copy
        {
        AddressLine1 = dto.AddressLine1 
        AddressLine2 = dto.AddressLine2 
        AddressLine3 = dto.AddressLine3 
        AddressLine4 = dto.AddressLine4 
        City = dto.City
        ZipCode = dto.ZipCode
        }

    /// Convert the DTO into a Address object
    /// Used when importing from the outside world into the domain, eg loading from a database.
    let toAddress (dto:AddressDto) :Result<Common.Address,string> =
        result {
            // get each (validated) simple type from the DTO as a success or failure
            let! addressLine1 = dto.AddressLine1 |> String50.create "AddressLine1"
            let! addressLine2 = dto.AddressLine2 |> String50.createOption "AddressLine2"
            let! addressLine3 = dto.AddressLine3 |> String50.createOption "AddressLine3"
            let! addressLine4 = dto.AddressLine4 |> String50.createOption "AddressLine4"
            let! city = dto.City |> String50.create "City"
            let! zipCode = dto.ZipCode |> ZipCode.create "ZipCode"

            // combine the components to create the domain object
            let address : Common.Address = {
                AddressLine1 = addressLine1 
                AddressLine2 = addressLine2 
                AddressLine3 = addressLine3 
                AddressLine4 = addressLine4 
                City = city
                ZipCode = zipCode
                }
            return address
            }

    /// Convert a Address object into the corresponding DTO.
    /// Used when exporting from the domain to the outside world.
    let fromAddress (domainObj:Common.Address) :AddressDto = 
        // this is a simple 1:1 copy
        {
        AddressLine1 = domainObj.AddressLine1 |> String50.value
        AddressLine2 = domainObj.AddressLine2 |> Option.map String50.value |> defaultIfNone null
        AddressLine3 = domainObj.AddressLine3 |> Option.map String50.value |> defaultIfNone null
        AddressLine4 = domainObj.AddressLine4 |> Option.map String50.value |> defaultIfNone null
        City = domainObj.City |> String50.value 
        ZipCode = domainObj.ZipCode |> ZipCode.value
        }


//===============================================
// DTOs for OrderLines
//===============================================

/// From the order form used as input
type OrderFormLineDto =  {
    OrderLineId : string
    ProductCode : string
    Quantity : decimal
    }

/// Functions relating to the OrderLine DTOs
module internal OrderLineDto =

    /// Convert the OrderFormLine into a UnvalidatedOrderLine  
    /// This always succeeds because there is no validation. 
    /// Used when importing an OrderForm from the outside world into the domain.
    let toUnvalidatedOrderLine (dto:OrderFormLineDto) :UnvalidatedOrderLine = 
        // this is a simple 1:1 copy
        {
        OrderLineId = dto.OrderLineId
        ProductCode = dto.ProductCode 
        Quantity = dto.Quantity
        }


//===============================================
// DTOs for PricedOrderLines
//===============================================

/// Used in the output of the workflow
type PricedOrderLineDto =  {
    OrderLineId : string
    ProductCode : string
    Quantity : decimal
    LinePrice : decimal
    }

module internal PricedOrderLineDto =
    /// Convert a PricedOrderLine object into the corresponding DTO.
    /// Used when exporting from the domain to the outside world.
    let fromDomain (domainObj:PricedOrderLine) :PricedOrderLineDto = 
        // this is a simple 1:1 copy
        {
        OrderLineId = domainObj.OrderLineId |> OrderLineId.value
        ProductCode = domainObj.ProductCode  |> ProductCode.value
        Quantity = domainObj.Quantity |> OrderQuantity.value
        LinePrice = domainObj.LinePrice |> Price.value
        }

//===============================================
// DTO for OrderForm
//===============================================

type OrderFormDto = {
    OrderId : string
    CustomerInfo : CustomerInfoDto
    ShippingAddress : AddressDto
    BillingAddress : AddressDto
    Lines : OrderFormLineDto list
    }

/// Functions relating to the Order DTOs
module internal OrderFormDto =

    /// Convert the OrderForm into a UnvalidatedOrder
    /// This always succeeds because there is no validation. 
    let toUnvalidatedOrder (dto:OrderFormDto) :UnvalidatedOrder = 
        {
        OrderId = dto.OrderId
        CustomerInfo = dto.CustomerInfo |> CustomerInfoDto.toUnvalidatedCustomerInfo
        ShippingAddress = dto.ShippingAddress |> AddressDto.toUnvalidatedAddress
        BillingAddress = dto.BillingAddress |> AddressDto.toUnvalidatedAddress
        Lines = dto.Lines |> List.map OrderLineDto.toUnvalidatedOrderLine
        }


//===============================================
// DTO for OrderPlaced event
//===============================================


/// Event to send to shipping context
type OrderPlacedDto = {
    OrderId : string
    CustomerInfo : CustomerInfoDto
    ShippingAddress : AddressDto
    BillingAddress : AddressDto
    AmountToBill : decimal
    Lines : PricedOrderLineDto list
    }

module internal OrderPlacedDto =

    /// Convert a OrderPlaced object into the corresponding DTO.
    /// Used when exporting from the domain to the outside world.
    let fromDomain (domainObj:OrderPlaced) :OrderPlacedDto = 
        {
        OrderId = domainObj.OrderId |> OrderId.value
        CustomerInfo = domainObj.CustomerInfo |> CustomerInfoDto.fromCustomerInfo
        ShippingAddress = domainObj.ShippingAddress |> AddressDto.fromAddress
        BillingAddress = domainObj.BillingAddress |> AddressDto.fromAddress
        AmountToBill = domainObj.AmountToBill |> BillingAmount.value
        Lines = domainObj.Lines |> List.map PricedOrderLineDto.fromDomain
        }

//===============================================
// DTO for BillableOrderPlaced event
//===============================================

/// Event to send to billing context
type BillableOrderPlacedDto = {
    OrderId : string
    BillingAddress: AddressDto
    AmountToBill : decimal
    }


module internal BillableOrderPlacedDto =

    /// Convert a BillableOrderPlaced object into the corresponding DTO.
    /// Used when exporting from the domain to the outside world.
    let fromDomain (domainObj:BillableOrderPlaced ) :BillableOrderPlacedDto = 
        {
        OrderId = domainObj.OrderId |> OrderId.value
        BillingAddress = domainObj.BillingAddress |> AddressDto.fromAddress
        AmountToBill = domainObj.AmountToBill |> BillingAmount.value
        }

//===============================================
// DTO for OrderAcknowledgmentSent event
//===============================================

/// Event to send to other bounded contexts
type OrderAcknowledgmentSentDto = {
    OrderId : string
    EmailAddress : string
    }


module internal OrderAcknowledgmentSentDto =

    /// Convert a OrderAcknowledgmentSent object into the corresponding DTO.
    /// Used when exporting from the domain to the outside world.
    let fromDomain (domainObj:OrderAcknowledgmentSent) :OrderAcknowledgmentSentDto = 
        {
        OrderId = domainObj.OrderId |> OrderId.value
        EmailAddress = domainObj.EmailAddress |> EmailAddress.value
        }

//===============================================
// DTO for PlaceOrderEvent
//===============================================

/// Use a dictionary representation of a PlaceOrderEvent, suitable for JSON
/// See "Serializing Records and Choice Types Using Maps" in chapter 11
type PlaceOrderEventDto = IDictionary<string,obj> 

module internal PlaceOrderEventDto = 

    /// Convert a PlaceOrderEvent into the corresponding DTO.
    /// Used when exporting from the domain to the outside world.
    let fromDomain (domainObj:PlaceOrderEvent) :PlaceOrderEventDto = 
        match domainObj with
        | OrderPlaced orderPlaced ->
            let obj = orderPlaced |> OrderPlacedDto.fromDomain |> box // use "box" to cast into an object
            let key = "OrderPlaced"
            [(key,obj)] |> dict
        | BillableOrderPlaced billableOrderPlaced ->
            let obj = billableOrderPlaced |> BillableOrderPlacedDto.fromDomain |> box
            let key = "BillableOrderPlaced"
            [(key,obj)] |> dict
        | AcknowledgmentSent orderAcknowledgmentSent ->
            let obj = orderAcknowledgmentSent |> OrderAcknowledgmentSentDto.fromDomain |> box
            let key = "OrderAcknowledgmentSent"
            [(key,obj)] |> dict

//===============================================
// DTO for PlaceOrderError
//===============================================

type PlaceOrderErrorDto = {
    Code : string
    Message : string
    }

module internal PlaceOrderErrorDto = 

    let fromDomain (domainObj:PlaceOrderError ) :PlaceOrderErrorDto = 
        match domainObj with
        | Validation validationError ->
            let (ValidationError msg) = validationError 
            {
                Code = "ValidationError"
                Message = msg
            }
        | Pricing pricingError ->
            let (PricingError msg) = pricingError 
            {
                Code = "PricingError"
                Message = msg
            }
        | RemoteService remoteServiceError ->
            let msg = sprintf "%s: %s" remoteServiceError.Service.Name remoteServiceError.Exception.Message
            {
                Code = "RemoteServiceError"
                Message = msg
            }



