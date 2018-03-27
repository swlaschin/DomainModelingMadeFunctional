module OrderTaking.PlaceOrder.Api

// ======================================================
// This file contains the JSON API interface to the PlaceOrder workflow
//
// 1) The HttpRequest is turned into a DTO, which is then turned into a Domain object
// 2) The main workflow function is called
// 3) The output is turned into a DTO which is turned into a HttpResponse
// ======================================================


open Newtonsoft.Json
open OrderTaking.Common
open OrderTaking.PlaceOrder


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

let checkProductExists : Implementation.CheckProductCodeExists =
    fun productCode -> 
        true // dummy implementation

let checkAddressExists : Implementation.CheckAddressExists =
    fun unvalidatedAddress -> 
        let checkedAddress = Implementation.CheckedAddress unvalidatedAddress 
        AsyncResult.retn checkedAddress 

let getProductPrice : Implementation.GetProductPrice =
    fun productCode -> 
        Price.unsafeCreate 1M  // dummy implementation


let createOrderAcknowledgmentLetter : Implementation.CreateOrderAcknowledgmentLetter =
    fun pricedOrder ->
        let letterTest = Implementation.HtmlString "some text"
        letterTest 

let sendOrderAcknowledgment : Implementation.SendOrderAcknowledgment =
    fun orderAcknowledgement ->
        Implementation.Sent 


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
                getProductPrice    // dependency
                createOrderAcknowledgmentLetter  // dependency
                sendOrderAcknowledgment // dependency

        // now we are in the pure domain
        let asyncResult = workflow unvalidatedOrder 

        // now convert from the pure domain back to a HttpResponse
        asyncResult 
        |> Async.map (workflowResultToHttpReponse)
