namespace FogentRoleplayLib

module Neg2To5 =
    type Neg2To5 =
        | NegTwo
        | NegOne
        | Zero
        | One
        | Two
        | Three
        | Four
        | Five

    let intToNeg2To5Option num =
        match num with
        | -2 -> Some NegTwo
        | -1 -> Some NegOne
        | 0 -> Some Zero
        | 1 -> Some One
        | 2 -> Some Two
        | 3 -> Some Three
        | 4 -> Some Four
        | 5 -> Some Five
        | _ -> None
    
    let neg2To5ToInt neg2To5 =
        match neg2To5 with
        | NegTwo -> -2
        | NegOne -> -1
        | Zero -> 0
        | One -> 1
        | Two -> 2
        | Three -> 3
        | Four -> 4
        | Five -> 5

module Attribute =
    type Attribute = string

module AttributeStat =
    open Neg2To5
    open Attribute

    type AttributeStat = {
        attribute: Attribute
        stat: Neg2To5
    }

module Neg1To5 =
    type Neg1To5 =
        | NegOne
        | Zero
        | One
        | Two
        | Three
        | Four
        | Five

    let intToNeg1To5Option num =
        match num with
        | -1 -> Some NegOne
        | 0 -> Some Zero
        | 1 -> Some One
        | 2 -> Some Two
        | 3 -> Some Three
        | 4 -> Some Four
        | 5 -> Some Five
        | _ -> None
    
    let neg1To5ToInt neg1To5 =
        match neg1To5 with
        | NegOne -> -1
        | Zero -> 0
        | One -> 1
        | Two -> 2
        | Three -> 3
        | Four -> 4
        | Five -> 5