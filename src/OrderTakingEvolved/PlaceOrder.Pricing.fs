namespace OrderTaking.PlaceOrder

// Move all procing logic into its own module,
// as it will likely get complicated!

open System
open OrderTaking.Common
open OrderTaking.PlaceOrder.InternalTypes


/// An internal helper module to help with pricing
module internal PricingModule = 


    /// Create a pricing method given a promotionCode on the unvalidated order form
    /// If null -> Standard otherwise wrap in PromotionCode
    let createPricingMethod (promotionCode:string) =
        if String.IsNullOrWhiteSpace promotionCode then
            Standard
        else
            Promotion (PromotionCode promotionCode)

    let getPricingFunction 
        (standardPrices:GetStandardPrices) 
        (promoPrices:GetPromotionPrices)  
        : GetPricingFunction = 
  

        // the original pricing function
        let getStandardPrice : GetProductPrice =
            // cache the standard prices		
            let getStandardPrices = standardPrices()
            // return the lookup function
            getStandardPrices 

        // the promotional pricing function
        let getPromotionPrice promotionCode : GetProductPrice =
            // cache the promotional prices
            let getPromotionPrice = promoPrices promotionCode
            // return the lookup function
            fun productCode ->
                match getPromotionPrice productCode with
                // found in promotional prices
                | Some price -> price 
                // not found in promotional prices
                // so use standard price
                | None -> getStandardPrice productCode

        // return a function that conforms to GetPricingFunction
        fun pricingMethod ->
            match pricingMethod with
            | Standard -> 
                getStandardPrice
            | Promotion promotionCode -> 
                getPromotionPrice promotionCode 
