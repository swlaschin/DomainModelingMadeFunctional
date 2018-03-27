module internal OrderTaking.PlaceOrder.Implementation

open OrderTaking.Common
open OrderTaking.PlaceOrder.InternalTypes

// ======================================================
// This file contains the final implementation for the PlaceOrderWorkflow
//
// This represents the code in chapter 10, "Working with Errors"
//
// There are two parts:
// * the first section contains the (type-only) definitions for each step
// * the second section contains the implementations for each step
//   and the implementation of the overall workflow
// ======================================================



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
        let! vipStatus = 
            unvalidatedCustomerInfo.VipStatus 
            |> VipStatus.create "vipStatus"
            |> Result.mapError ValidationError // convert creation error into ValidationError

        let customerInfo = {
            Name = {FirstName=firstName; LastName=lastName}
            EmailAddress = emailAddress
            VipStatus = vipStatus 
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
        let! state = 
            unvalidatedAddress.State
            |> UsStateCode.create "State"
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let! country = 
            unvalidatedAddress.Country 
            |> String50.create "Country"
            |> Result.mapError ValidationError // convert creation error into ValidationError
        let address : Address = {
            AddressLine1 = addressLine1
            AddressLine2 = addressLine2
            AddressLine3 = addressLine3
            AddressLine4 = addressLine4
            City = city
            ZipCode = zipCode
            State = state
            Country = country
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
        let validatedOrderLine : ValidatedOrderLine = {
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
            let pricingMethod = 
                unvalidatedOrder.PromotionCode
                |> PricingModule.createPricingMethod 

            let validatedOrder : ValidatedOrder = {
                OrderId  = orderId 
                CustomerInfo = customerInfo 
                ShippingAddress = shippingAddress 
                BillingAddress = billingAddress  
                Lines = lines 
                PricingMethod = pricingMethod 
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
        let pricedLine : PricedOrderProductLine = {
            OrderLineId = validatedOrderLine.OrderLineId 
            ProductCode = validatedOrderLine.ProductCode 
            Quantity = validatedOrderLine.Quantity
            LinePrice = linePrice
            }
        return (ProductLine pricedLine)
    }


// add the special comment line if needed
let addCommentLine pricingMethod lines =
    match pricingMethod with
    | Standard -> 
        // unchanged
        lines 
    | Promotion (PromotionCode promoCode) ->  
        let commentLine = 
            sprintf "Applied promotion %s" promoCode 
            |> CommentLine // lift to PricedOrderLine
        List.append lines [commentLine]

let getLinePrice line =
    match line with
    | ProductLine line ->
        line.LinePrice
    | CommentLine _ ->
        Price.unsafeCreate 0M

let priceOrder : PriceOrder = 
    fun getPricingFunction validatedOrder ->
        let getProductPrice = getPricingFunction validatedOrder.PricingMethod
        result {
            let! lines = 
                validatedOrder.Lines 
                |> List.map (toPricedOrderLine getProductPrice) 
                |> Result.sequence // convert list of Results to a single Result
                |> Result.map (fun lines ->
                    lines |> addCommentLine validatedOrder.PricingMethod 
                    )

            let! amountToBill = 
                lines 
                |> List.map getLinePrice                   // get each line price
                |> BillingAmount.sumPrices                // add them together as a BillingAmount
                |> Result.mapError PricingError           // convert to PlaceOrderError
            let pricedOrder : PricedOrder = {
                OrderId  = validatedOrder.OrderId 
                CustomerInfo = validatedOrder.CustomerInfo 
                ShippingAddress = validatedOrder.ShippingAddress 
                BillingAddress = validatedOrder.BillingAddress  
                Lines = lines 
                AmountToBill = amountToBill 
                PricingMethod = validatedOrder.PricingMethod 
            }
            return pricedOrder 
        }
        

// ---------------------------
// Shipping step
// ---------------------------

let (|UsLocalState|UsRemoteState|International|) (address:Address) = 
    if address.Country |> String50.value = "US" then
        match address.State |> UsStateCode.value  with
        | "CA" | "OR" | "AZ" | "NV" -> 
            UsLocalState
        | _ -> 
            UsRemoteState
    else
        International

let calculateShippingCost : CalculateShippingCost =
    fun pricedOrder ->
        match pricedOrder.ShippingAddress with
            | UsLocalState -> 5.0M
            | UsRemoteState -> 10.0M
            | International -> 20.0M
        |> Price.unsafeCreate

let addShippingInfoToOrder : AddShippingInfoToOrder  =
    fun calculateShippingCost pricedOrder ->
        // create the shipping info
        let shippingInfo = {
            ShippingMethod = Fedex24
            ShippingCost = calculateShippingCost pricedOrder 
            }

        // add it to the order
        {
        PricedOrder = pricedOrder
        ShippingInfo = shippingInfo
        }

// ---------------------------
// VIP shipping step
// ---------------------------

/// Update the shipping cost if customer is VIP
let freeVipShipping : FreeVipShipping =
    fun order -> 
        let updatedShippingInfo = 
            match order.PricedOrder.CustomerInfo.VipStatus with
            | Normal -> 
                // untouched
                order.ShippingInfo
            | Vip -> 
                {order.ShippingInfo with 
                    ShippingCost = Price.unsafeCreate 0.0M
                    ShippingMethod = Fedex24 }

        {order with ShippingInfo = updatedShippingInfo }


// ---------------------------
// AcknowledgeOrder step
// ---------------------------

let acknowledgeOrder : AcknowledgeOrder = 
    fun createAcknowledgmentLetter sendAcknowledgment pricedOrderWithShipping ->
        let pricedOrder = pricedOrderWithShipping.PricedOrder

        let letter = createAcknowledgmentLetter pricedOrderWithShipping
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

let makeShipmentLine (line: PricedOrderLine) : ShippableOrderLine option =
    match line with
    | ProductLine line ->
        {
        ProductCode = line.ProductCode
        Quantity = line.Quantity
        } |> Some
    | CommentLine _ ->
        None


let createShippingEvent (placedOrder:PricedOrder) : ShippableOrderPlaced =
    {
    OrderId = placedOrder.OrderId
    ShippingAddress = placedOrder.ShippingAddress
    ShipmentLines = placedOrder.Lines |> List.choose makeShipmentLine
    Pdf = 
        { 
        Name = sprintf "Order%s.pdf" (placedOrder.OrderId |> OrderId.value)
        Bytes = [||]
        }
    } 
 
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
        let shippingEvents = 
            pricedOrder
            |> createShippingEvent
            |> PlaceOrderEvent.ShippableOrderPlaced
            |> List.singleton
        let billingEvents = 
            pricedOrder
            |> createBillingEvent 
            |> Option.map PlaceOrderEvent.BillableOrderPlaced
            |> listOfOption

        // return all the events
        [
        yield! acknowledgmentEvents
        yield! shippingEvents 
        yield! billingEvents
        ]            


// ---------------------------
// overall workflow
// ---------------------------

let placeOrder 
    checkProductExists // dependency
    checkAddressExists // dependency
    getProductPrice    // dependency
    calculateShippingCost // dependency
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
            let pricedOrderWithShipping = 
                pricedOrder 
                |> addShippingInfoToOrder calculateShippingCost
                |> freeVipShipping
            let acknowledgementOption = 
                acknowledgeOrder createOrderAcknowledgmentLetter sendOrderAcknowledgment pricedOrderWithShipping 
            let events = 
                createEvents pricedOrder acknowledgementOption 
            return events
        }
