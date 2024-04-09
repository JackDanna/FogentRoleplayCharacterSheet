namespace FogentRoleplayLib

module TypeUtils =

    let stringListToTypeMap (stringTypeArray: string list) =
        List.zip stringTypeArray stringTypeArray |> Map.ofList

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

    let emptyDicePool = {
        d4 = 0u
        d6 = 0u
        d8 = 0u
        d10 = 0u
        d12 = 0u
        d20 = 0u
    }

    let baseDicePool = { emptyDicePool with d6 = 3u }

    let diceToString numDice diceTypeString =
        if numDice <> 0u then
            string numDice + diceTypeString
        else
            ""

    let checkIfEmptyDicePoolString dicePoolString =
        if dicePoolString = "" then "0d6" else dicePoolString

    let dicePoolToString (dicePool: DicePool) =
        [
            diceToString dicePool.d4 "d4"
            diceToString dicePool.d6 "d6"
            diceToString dicePool.d8 "d8"
            diceToString dicePool.d10 "d10"
            diceToString dicePool.d12 "d12"
            diceToString dicePool.d20 "d20"
        ]
        |> List.filter (fun diceString -> diceString <> "")
        |> String.concat ", "
        |> checkIfEmptyDicePoolString

    let combineDicePools dicePools =
        List.fold
            (fun acc pool -> {
                d4 = acc.d4 + pool.d4
                d6 = acc.d6 + pool.d6
                d8 = acc.d8 + pool.d8
                d10 = acc.d10 + pool.d10
                d12 = acc.d12 + pool.d12
                d20 = acc.d20 + pool.d20
            })
            emptyDicePool
            dicePools

    let dicePoolToNumDice (dicePool: DicePool) =
        let {
                d4 = d4
                d6 = d6
                d8 = d8
                d10 = d10
                d12 = d12
                d20 = d20
            } =
            dicePool

        d4 + d6 + d8 + d10 + d12 + d20

module DicePoolMod =
    open DicePool

    type DicePoolPenalty = uint // always should deduct the dice with the fewest faces first (i.e. d4, then d6, then d8...)

    type DicePoolMod =
        | AddDice of DicePool
        | RemoveDice of DicePoolPenalty

    let createD6DicePoolMod (numDice: uint) =
        AddDice { emptyDicePool with d6 = numDice }

    let removeDice (dice: uint) (neg: uint) : uint * DicePoolPenalty =
        let result = int dice - int neg
        // If the result is negative, their are still dice to lose, but they are of a higher face value
        if result < 0 then
            (0u, uint (abs result))
        // Else the result is 0 or positive, there are no more dice to lose
        else
            (uint result, 0u)

    let removeDiceFromDicePool (dicePool: DicePool) (dicePoolPenalty: DicePoolPenalty) =
        let d4, d6Neg = removeDice dicePool.d4 dicePoolPenalty
        let d6, d8Neg = removeDice dicePool.d6 d6Neg
        let d8, d10Neg = removeDice dicePool.d8 d8Neg
        let d10, d12Neg = removeDice dicePool.d10 d10Neg
        let d12, d20Neg = removeDice dicePool.d12 d12Neg
        let d20, _ = removeDice dicePool.d20 d20Neg

        {
            d4 = d4
            d6 = d6
            d8 = d8
            d10 = d10
            d12 = d12
            d20 = d20
        }

    let modifyDicePool (dicePool: DicePool) (dicePoolMod: DicePoolMod) : DicePool =
        match dicePoolMod with
        | AddDice diceToAdd -> combineDicePools [ dicePool; diceToAdd ]
        | RemoveDice diceToRemove -> removeDiceFromDicePool dicePool diceToRemove

    let dicePoolModToInt dicePoolMod =
        match dicePoolMod with
        | AddDice dicePool -> dicePoolToNumDice dicePool |> int
        | RemoveDice dicePoolPenalty -> int dicePoolPenalty * -1

    let modifyDicePoolByDicePoolModList dicePool dicePoolMods =

        let combinedDicePoolPenalty =
            List.fold
                (fun acc diceMod ->
                    match diceMod with
                    | RemoveDice dicePoolPenalty -> acc + dicePoolPenalty
                    | _ -> acc)
                0u
                dicePoolMods
            |> RemoveDice

        let combinedPositiveDicePool =
            List.fold
                (fun acc diceMod ->
                    match diceMod with
                    | AddDice dicePool -> combineDicePools [ acc; dicePool ]
                    | _ -> acc)
                dicePool
                dicePoolMods

        // Does the subtractions only at the end after combining
        modifyDicePool combinedPositiveDicePool combinedDicePoolPenalty

    let intToD6DicePoolMod (num: int) =
        if num < 0 then
            RemoveDice(uint (abs num))
        else
            createD6DicePoolMod (uint num)


    let dicePoolModToString dicePoolMod =
        match dicePoolMod with
        | RemoveDice removeDice -> $"-{uint removeDice}d"
        | AddDice addDice -> dicePoolToString addDice

    // Parsing Logic
    let createDicePoolMod (numDiceStr: string) (diceType: string) =
        let numDice = uint numDiceStr

        match diceType with
        | "4" -> { emptyDicePool with d4 = numDice }
        | "6" -> { emptyDicePool with d6 = numDice }
        | "8" -> { emptyDicePool with d8 = numDice }
        | "10" -> { emptyDicePool with d10 = numDice }
        | "12" -> { emptyDicePool with d12 = numDice }
        | "20" -> { emptyDicePool with d20 = numDice }
        | _ -> emptyDicePool

    let parseDicePoolString (dicePoolString: string) =
        dicePoolString.Split ", "
        |> List.ofArray
        |> List.map (fun (diceStr) ->
            let diceNumAndDiceType = diceStr.Split "d"
            createDicePoolMod diceNumAndDiceType[0] diceNumAndDiceType[1])
        |> combineDicePools

    let parseDicePoolModString (dicePoolModString: string) : DicePoolMod =
        if dicePoolModString.Contains("+") then
            let str = dicePoolModString.Replace("+", "")
            AddDice <| parseDicePoolString str
        elif dicePoolModString.Contains("-") then
            let removeDiceString = dicePoolModString.Replace("-", "")

            match System.UInt32.TryParse(removeDiceString) with
            | (true, result) -> RemoveDice result
            | _ -> RemoveDice 0u
        else
            RemoveDice 0u

    let parseDicePoolModOptionString (dicePoolJSONString: string) : DicePoolMod option =
        match dicePoolJSONString with
        | "None" -> None
        | modString -> Some <| parseDicePoolModString modString

module Skill =
    open Neg1To5
    open DicePool
    open DicePoolMod

    type Skill = {
        name: string
        level: Neg1To5
        dicePool: DicePool
    }

    type DicePoolCalculationData = {
        baseDice: DicePool option
        attributeDicePoolMod: DicePoolMod
        injuryDicePenalty: DicePoolPenalty
        weightClassDicePenalty: DicePoolPenalty
        itemEffectDicePoolMod: DicePoolMod
    }

module CoreSkill =
    open Attribute
    open Skill
    open DicePoolMod
    open DicePool
    open Neg1To5

    type CoreSkill = {
        skill: Skill
        governingAttribute: Attribute
    }

    let calculateCoreSkillDicePool (dicePoolCalculationData: DicePoolCalculationData) (skillLevel: Neg1To5) =
        modifyDicePoolByDicePoolModList (dicePoolCalculationData.baseDice |> Option.defaultValue baseDicePool) [
            skillLevel |> neg1To5ToInt |> intToD6DicePoolMod
            dicePoolCalculationData.injuryDicePenalty |> RemoveDice
            dicePoolCalculationData.itemEffectDicePoolMod
            dicePoolCalculationData.weightClassDicePenalty |> RemoveDice
        ]

module AttributeStat =
    open Neg2To5
    open Attribute

    type AttributeStat = { attribute: Attribute; stat: Neg2To5 }

module AttributeAndCoreSkills =
    open Attribute
    open Neg2To5
    open AttributeStat
    open CoreSkill

    type AttributeAndCoreSkills = {
        attribute: AttributeStat
        coreSkills: CoreSkill list
    }

    let defaultAttributeAndCoreSkills (coreSkillList: CoreSkill list) (attribute: Attribute) : AttributeAndCoreSkills = {
        attribute = { attribute = attribute; stat = Zero }
        coreSkills =
            List.collect
                (fun coreSkill ->
                    if coreSkill.governingAttribute = attribute then
                        [ coreSkill ]
                    else
                        [])
                coreSkillList
    }

module Character =
    open Attribute
    open CoreSkill
    open AttributeAndCoreSkills

    type Character = {
        name: string
        attributeAndCoreSkillsList: AttributeAndCoreSkills list
    }

    let defaultAttributeAndCoreSkillsList (attributeList: Attribute list) (coreSkillList: CoreSkill list) =
        List.map (defaultAttributeAndCoreSkills coreSkillList) attributeList