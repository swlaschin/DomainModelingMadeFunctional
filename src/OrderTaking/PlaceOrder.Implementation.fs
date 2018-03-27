module internal OrderTaking.PlaceOrder.Implementation

open OrderTaking.Common

// ======================================================
// This file contains the final implementation for the PlaceOrder workflow
//
// This represents the code in chapter 10, "Working with Errors"
//
// There are two parts:
// * the first section contains the (type-only) definitions for each step
// * the second section contains the implementations for each step
//   and the implementation of the overall workflow
// ======================================================


// ======================================================
// Section 1 : Define each step in the workflow using types
// ======================================================

// ---------------------------
// Validation step
// ---------------------------

// Product validation

type CheckProductCodeExists = 
    ProductCode -> bool

// Address validation
type AddressValidationError = 
    | InvalidFormat 
    | AddressNotFound 

type CheckedAddress = CheckedAddress of UnvalidatedAddress

type CheckAddressExists = 
    UnvalidatedAddress -> AsyncResult<CheckedAddress,AddressValidationError>

// ---------------------------
// Validated Order 
// ---------------------------

type ValidatedOrderLine =  {
    OrderLineId : OrderLineId 
    ProductCode : ProductCode 
    Quantity : OrderQuantity
    }

type ValidatedOrder = {
    OrderId : OrderId
    CustomerInfo : CustomerInfo
    ShippingAddress : Address
    BillingAddress : Address
    Lines : ValidatedOrderLine list
    }

type ValidateOrder = 
    CheckProductCodeExists  // dependency
     -> CheckAddressExists  // dependency
     -> UnvalidatedOrder    // input
     -> AsyncResult<ValidatedOrder, ValidationError> // output

// ---------------------------
// Pricing step
// ---------------------------

type GetProductPrice = 
    ProductCode -> Price

// priced state is defined Domain.WorkflowTypes

type PriceOrder = 
    GetProductPrice     // dependency
     -> ValidatedOrder  // input
     -> Result<PricedOrder, PricingError>  // output


// ---------------------------
// Send OrderAcknowledgment 
// ---------------------------

type HtmlString = 
    HtmlString of string

type OrderAcknowledgment = {
    EmailAddress : EmailAddress
    Letter : HtmlString 
    }

type CreateOrderAcknowledgmentLetter =
    PricedOrder -> HtmlString

/// Send the order acknowledgement to the customer
/// Note that this does NOT generate an Result-type error (at least not in this workflow)
/// because on failure we will continue anyway.
/// On success, we will generate a OrderAcknowledgmentSent event,
/// but on failure we won't.

type SendResult = Sent | NotSent

type SendOrderAcknowledgment =
    OrderAcknowledgment -> SendResult 
    
type AcknowledgeOrder = 
    CreateOrderAcknowledgmentLetter  // dependency
     -> SendOrderAcknowledgment      // dependency
     -> PricedOrder                  // input
     -> OrderAcknowledgmentSent option // output

// ---------------------------
// Create events
// ---------------------------

type CreateEvents = 
    PricedOrder                           // input
     -> OrderAcknowledgmentSent option    // input (event from previous step)
     -> PlaceOrderEvent list              // output


// ======================================================
// Section 2 : Implementation
// ======================================================

// ---------------------------
// ValidateOrder step
// ---------------------------

let toCustomerInfo (unvalidatedCustomerInfo: UnvalidatedCustomerInfo) =
    result {
        let! firstName = 
            unvalidatedCustomerInfo.FirstName
            |> String50.create "FirstName"
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let! lastName = 
            unvalidatedCustomerInfo.LastName
            |> String50.create "LastName"
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let! emailAddress = 
            unvalidatedCustomerInfo.EmailAddress
            |> EmailAddress.create "EmailAddress"
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let customerInfo = {
            Name = {FirstName=firstName; LastName=lastName}
            EmailAddress = emailAddress
            }
        return customerInfo 
    }

let toAddress (CheckedAddress unvalidatedAddress) =
    result {
        let! addressLine1 = 
            unvalidatedAddress.AddressLine1 
            |> String50.create "AddressLine1" 
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let! addressLine2 = 
            unvalidatedAddress.AddressLine2 
            |> String50.createOption "AddressLine2"
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let! addressLine3 = 
            unvalidatedAddress.AddressLine3 
            |> String50.createOption "AddressLine3" 
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let! addressLine4 = 
            unvalidatedAddress.AddressLine4 
            |> String50.createOption "AddressLine4"
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let! city = 
            unvalidatedAddress.City
            |> String50.create "City"
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let! zipCode = 
            unvalidatedAddress.ZipCode
            |> ZipCode.create "ZipCode"
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let address : Address = {
            AddressLine1 = addressLine1
            AddressLine2 = addressLine2
            AddressLine3 = addressLine3
            AddressLine4 = addressLine4
            City = city
            ZipCode = zipCode
            }
        return address
    }

/// Call the checkAddressExists and convert the error to a ValidationError
let toCheckedAddress (checkAddress:CheckAddressExists) address =
    address 
    |> checkAddress 
    |> AsyncResult.mapError (fun addrError -> 
        match addrError with
        | AddressNotFound -> ValidationError "Address not found"
        | InvalidFormat -> ValidationError "Address has bad format"
        )

let toOrderId orderId = 
    orderId 
    |> OrderId.create "OrderId"
    |> Result.mapError ValidationError // convert creation error into ValidationError

/// Helper function for validateOrder   
let toOrderLineId orderId = 
    orderId 
    |> OrderLineId.create "OrderLineId"
    |> Result.mapError ValidationError // convert creation error into ValidationError

/// Helper function for validateOrder   
let toProductCode (checkProductCodeExists:CheckProductCodeExists) productCode = 

    // create a ProductCode -> Result<ProductCode,...> function 
    // suitable for using in a pipeline
    let checkProduct productCode  = 
        if checkProductCodeExists productCode then
            Ok productCode 
        else
            let msg = sprintf "Invalid: %A" productCode 
            Error (ValidationError msg) 
        
    // assemble the pipeline        
    productCode
    |> ProductCode.create "ProductCode"
    |> Result.mapError ValidationError // convert creation error into ValidationError
    |> Result.bind checkProduct 

/// Helper function for validateOrder   
let toOrderQuantity productCode quantity = 
    OrderQuantity.create "OrderQuantity" productCode quantity  
    |> Result.mapError ValidationError // convert creation error into ValidationError
   
/// Helper function for validateOrder   
let toValidatedOrderLine checkProductExists (unvalidatedOrderLine:UnvalidatedOrderLine) = 
    result {
        let! orderLineId = 
            unvalidatedOrderLine.OrderLineId 
            |> toOrderLineId
        let! productCode = 
            unvalidatedOrderLine.ProductCode 
            |> toProductCode checkProductExists
        let! quantity = 
            unvalidatedOrderLine.Quantity 
            |> toOrderQuantity productCode 
        let validatedOrderLine = {
            OrderLineId = orderLineId 
            ProductCode = productCode 
            Quantity = quantity 
            }
        return validatedOrderLine 
    }

let validateOrder : ValidateOrder = 
    fun checkProductCodeExists checkAddressExists unvalidatedOrder ->
        asyncResult {
            let! orderId = 
                unvalidatedOrder.OrderId 
                |> toOrderId
                |> AsyncResult.ofResult
            let! customerInfo = 
                unvalidatedOrder.CustomerInfo 
                |> toCustomerInfo
                |> AsyncResult.ofResult
            let! checkedShippingAddress = 
                unvalidatedOrder.ShippingAddress 
                |> toCheckedAddress checkAddressExists
            let! shippingAddress = 
                checkedShippingAddress 
                |> toAddress 
                |> AsyncResult.ofResult
            let! checkedBillingAddress = 
                unvalidatedOrder.BillingAddress 
                |> toCheckedAddress checkAddressExists
            let! billingAddress  = 
                checkedBillingAddress
                |> toAddress 
                |> AsyncResult.ofResult
            let! lines = 
                unvalidatedOrder.Lines 
                |> List.map (toValidatedOrderLine checkProductCodeExists) 
                |> Result.sequence // convert list of Results to a single Result
                |> AsyncResult.ofResult
            let validatedOrder : ValidatedOrder = {
                OrderId  = orderId 
                CustomerInfo = customerInfo 
                ShippingAddress = shippingAddress 
                BillingAddress = billingAddress  
                Lines = lines 
            }
            return validatedOrder 
        }

// ---------------------------
// PriceOrder step
// ---------------------------

let toPricedOrderLine (getProductPrice:GetProductPrice) (validatedOrderLine:ValidatedOrderLine) = 
    result {
        let qty = validatedOrderLine.Quantity |> OrderQuantity.value 
        let price = validatedOrderLine.ProductCode |> getProductPrice 
        let! linePrice = 
            Price.multiply qty price 
            |> Result.mapError PricingError // convert to PlaceOrderError
        let pricedLine : PricedOrderLine = {
            OrderLineId = validatedOrderLine.OrderLineId 
            ProductCode = validatedOrderLine.ProductCode 
            Quantity = validatedOrderLine.Quantity
            LinePrice = linePrice
            }
        return pricedLine
    }


let priceOrder : PriceOrder = 
    fun getProductPrice validatedOrder ->
        result {
            let! lines = 
                validatedOrder.Lines 
                |> List.map (toPricedOrderLine getProductPrice) 
                |> Result.sequence // convert list of Results to a single Result
            let! amountToBill = 
                lines 
                |> List.map (fun line -> line.LinePrice)  // get each line price
                |> BillingAmount.sumPrices                // add them together as a BillingAmount
                |> Result.mapError PricingError           // convert to PlaceOrderError
            let pricedOrder : PricedOrder = {
                OrderId  = validatedOrder.OrderId 
                CustomerInfo = validatedOrder.CustomerInfo 
                ShippingAddress = validatedOrder.ShippingAddress 
                BillingAddress = validatedOrder.BillingAddress  
                Lines = lines 
                AmountToBill = amountToBill 
            }
            return pricedOrder 
        }
        

// ---------------------------
// AcknowledgeOrder step
// ---------------------------

let acknowledgeOrder : AcknowledgeOrder = 
    fun createAcknowledgmentLetter sendAcknowledgment pricedOrder ->
        let letter = createAcknowledgmentLetter pricedOrder
        let acknowledgment = {
            EmailAddress = pricedOrder.CustomerInfo.EmailAddress
            Letter = letter 
            }

        // if the acknowledgement was successfully sent,
        // return the corresponding event, else return None
        match sendAcknowledgment acknowledgment with
        | Sent -> 
            let event = {
                OrderId = pricedOrder.OrderId
                EmailAddress = pricedOrder.CustomerInfo.EmailAddress
                } 
            Some event
        | NotSent ->
            None

// ---------------------------
// Create events
// ---------------------------

let createOrderPlacedEvent (placedOrder:PricedOrder) : OrderPlaced =
    placedOrder
 
let createBillingEvent (placedOrder:PricedOrder) : BillableOrderPlaced option =
    let billingAmount = placedOrder.AmountToBill |> BillingAmount.value
    if billingAmount > 0M then
        {
        OrderId = placedOrder.OrderId
        BillingAddress = placedOrder.BillingAddress
        AmountToBill = placedOrder.AmountToBill 
        } |> Some
    else
        None

/// helper to convert an Option into a List
let listOfOption opt =
    match opt with 
    | Some x -> [x]
    | None -> []

let createEvents : CreateEvents = 
    fun pricedOrder acknowledgmentEventOpt ->
        let acknowledgmentEvents = 
            acknowledgmentEventOpt 
            |> Option.map PlaceOrderEvent.AcknowledgmentSent
            |> listOfOption
        let orderPlacedEvents = 
            pricedOrder
            |> createOrderPlacedEvent
            |> PlaceOrderEvent.OrderPlaced
            |> List.singleton
        let billingEvents = 
            pricedOrder
            |> createBillingEvent 
            |> Option.map PlaceOrderEvent.BillableOrderPlaced
            |> listOfOption

        // return all the events
        [
        yield! acknowledgmentEvents
        yield! orderPlacedEvents 
        yield! billingEvents
        ]            


// ---------------------------
// overall workflow
// ---------------------------

let placeOrder 
    checkProductExists // dependency
    checkAddressExists // dependency
    getProductPrice    // dependency
    createOrderAcknowledgmentLetter  // dependency
    sendOrderAcknowledgment // dependency
    : PlaceOrder =       // definition of function

    fun unvalidatedOrder -> 
        asyncResult {
            let! validatedOrder = 
                validateOrder checkProductExists checkAddressExists unvalidatedOrder 
                |> AsyncResult.mapError PlaceOrderError.Validation
            let! pricedOrder = 
                priceOrder getProductPrice validatedOrder 
                |> AsyncResult.ofResult
                |> AsyncResult.mapError PlaceOrderError.Pricing
            let acknowledgementOption = 
                acknowledgeOrder createOrderAcknowledgmentLetter sendOrderAcknowledgment pricedOrder 
            let events = 
                createEvents pricedOrder acknowledgementOption 
            return events
        }
