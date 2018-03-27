module OrderTaking.PlaceOrder.Api

// ======================================================
// This file contains the complete workflow, exposed as a JSON API
//
// 1) The HttpRequest is turned into a DTO, which is then turned into a Domain object
// 2) The main workflow function is called
// 3) The output is turned into a DTO which is turned into a HttpResponse
// ======================================================



open Newtonsoft.Json
open OrderTaking.Common
open OrderTaking.PlaceOrder.InternalTypes


type JsonString = string

/// Very simplified version!
type HttpRequest = {
    Action : string
    Uri : string
    Body : JsonString 
    }

/// Very simplified version!
type HttpResponse = {
    HttpStatusCode : int
    Body : JsonString 
    }

/// An API takes a HttpRequest as input and returns a async response
type PlaceOrderApi = HttpRequest -> Async<HttpResponse>


// =============================
// Implementation
// =============================

// setup dummy dependencies            

let checkProductExists : CheckProductCodeExists =
    fun productCode -> 
        true // dummy implementation

let checkAddressExists : CheckAddressExists =
    fun unvalidatedAddress -> 
        let checkedAddress = CheckedAddress unvalidatedAddress 
        AsyncResult.retn checkedAddress 

let getStandardPrices() : GetProductPrice =
    fun productCode -> 
        Price.unsafeCreate 10M 

let getPromotionPrices (PromotionCode promotionCode) :TryGetProductPrice =

    let halfPricePromotion : TryGetProductPrice = 
        fun productCode -> 
            if ProductCode.value productCode = "ONSALE" then
                Price.unsafeCreate 5M |> Some
            else
                None

    let quarterPricePromotion : TryGetProductPrice = 
        fun productCode -> 
            if ProductCode.value productCode = "ONSALE" then
                Price.unsafeCreate 2.5M |> Some
            else
                None

    let noPromotion : TryGetProductPrice = 
        fun productCode -> None

    match promotionCode with
    | "HALF" -> halfPricePromotion
    | "QUARTER" -> quarterPricePromotion
    | _ -> noPromotion 

let getPricingFunction :GetPricingFunction = 
    PricingModule.getPricingFunction getStandardPrices  getPromotionPrices 
  
let calculateShippingCost = 
    Implementation.calculateShippingCost

let createOrderAcknowledgmentLetter : CreateOrderAcknowledgmentLetter =
    fun pricedOrder ->
        let letterTest = HtmlString "some text"
        letterTest 

let sendOrderAcknowledgment : SendOrderAcknowledgment =
    fun orderAcknowledgement ->
        Sent 


// -------------------------------
// workflow
// -------------------------------

/// This function converts the workflow output into a HttpResponse
let workflowResultToHttpReponse result = 
    match result with
    | Ok events ->
        // turn domain events into dtos
        let dtos = 
            events 
            |> List.map PlaceOrderEventDto.fromDomain
            |> List.toArray // arrays are json friendly
        // and serialize to JSON
        let json = JsonConvert.SerializeObject(dtos)
        let response = 
            {
            HttpStatusCode = 200
            Body = json
            }
        response
    | Error err -> 
        // turn domain errors into a dto
        let dto = err |> PlaceOrderErrorDto.fromDomain
        // and serialize to JSON
        let json = JsonConvert.SerializeObject(dto )
        let response = 
            {
            HttpStatusCode = 401
            Body = json
            }
        response

let placeOrderApi : PlaceOrderApi =
    fun request ->
        // following the approach in "A Complete Serialization Pipeline" in chapter 11

        // start with a string
        let orderFormJson = request.Body
        let orderForm = JsonConvert.DeserializeObject<OrderFormDto>(orderFormJson)
        // convert to domain object
        let unvalidatedOrder = orderForm |> OrderFormDto.toUnvalidatedOrder

        // setup the dependencies. See "Injecting Dependencies" in chapter 9
        let workflow = 
            Implementation.placeOrder 
                checkProductExists // dependency
                checkAddressExists // dependency
                getPricingFunction // dependency
                calculateShippingCost // dependency
                createOrderAcknowledgmentLetter  // dependency
                sendOrderAcknowledgment // dependency

        // now we are in the pure domain
        let asyncResult = workflow unvalidatedOrder 

        // now convert from the pure domain back to a HttpResponse
        asyncResult 
        |> Async.map (workflowResultToHttpReponse)
