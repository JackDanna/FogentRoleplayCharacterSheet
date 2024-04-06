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

module DicePool =
    type DicePool = {
        d4: uint
        d6: uint
        d8: uint
        d10: uint
        d12: uint
        d20: uint
    }

    let emptyDicePool =
        { d4 = 0u
          d6 = 0u
          d8 = 0u
          d10 = 0u
          d12 = 0u
          d20 = 0u }
    
    let baseDicePool = { emptyDicePool with d6 = 3u }

    let diceToString numDice diceTypeString =
        if numDice <> 0u then
            string numDice + diceTypeString
        else
            ""

    let checkIfEmptyDicePoolString dicePoolString =
        if dicePoolString = "" then
            "0d6"
        else
            dicePoolString

    let dicePoolToString (dicePool: DicePool) =
        [ diceToString dicePool.d4 "d4"
          diceToString dicePool.d6 "d6"
          diceToString dicePool.d8 "d8"
          diceToString dicePool.d10 "d10"
          diceToString dicePool.d12 "d12"
          diceToString dicePool.d20 "d20" ]
        |> List.filter (fun diceString -> diceString <> "")
        |> String.concat ", "
        |> checkIfEmptyDicePoolString