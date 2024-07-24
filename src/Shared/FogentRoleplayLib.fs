namespace FogentRoleplayLib
// Utils
module ParsingUtils =

    let commaAndSpace = ", "

    let commaSeperatedStringToArray (input: string) =
        input.Split(commaAndSpace, System.StringSplitOptions.RemoveEmptyEntries)
    // |> String.filter ((<>) ' ')
    // |> (fun s -> s.Split(',', System.StringSplitOptions.RemoveEmptyEntries))

    let commaSeperatedStringToSet = commaSeperatedStringToArray >> Set.ofArray

    let commaSeperatedStringToList = commaSeperatedStringToArray >> List.ofArray

module StringUtils =
    open ParsingUtils
    open System.Text.RegularExpressions

    let emptyString = ""

    let isNumeric (number: string) =
        let regex = Regex(@"^[0-9]+$")
        regex.IsMatch(number)

    let stringSeqToStringSeperatedByCommaAndSpace (stringSeq: string seq) = String.concat commaAndSpace stringSeq

    let stringSetToStringSeperatedByCommas stringSet =
        stringSet |> List.ofSeq |> stringSeqToStringSeperatedByCommaAndSpace

    let mapAndStringToValueSet (map: Map<string, 'a>) (input: string) =
        if input.Length = 0 then
            Set.empty
        else
            commaSeperatedStringToSet input
            |> Set.map (fun attributeString -> map.Item attributeString)

module MathUtils =
    open System

    let divideUintByUintThenRound numerator divisor roundDown =
        let floatCalculation = float numerator / float divisor

        match roundDown with
        | true -> Math.Floor floatCalculation |> uint
        | false -> Math.Ceiling floatCalculation |> uint

    let divideUintsThenCompareToMaxThenRound
        (numerator: uint)
        (divisor: uint)
        (maxAllowableValue: uint option)
        roundDown
        =
        let result = divideUintByUintThenRound numerator divisor roundDown

        match maxAllowableValue with
        | Some max -> if (max < result) then max else result
        | None -> result

    let roundDownToNearestMultipleOf5 (value: float) : uint = Math.Floor(value / 5.0) * 5.0 |> uint

module TypeUtils =

    let stringSetToTypeMap (stringTypeArray: string Set) =
        Seq.zip stringTypeArray stringTypeArray |> Map.ofSeq

// Base Building Blocks

module Penetration =

    type Penetration = uint

module DamageType =

    type DamageType = string

module EngageableOpponents =

    open MathUtils
    open StringUtils

    type EngageableOpponentsCalculation = {
        name: string
        combatRollDivisor: uint
        maxEOOption: uint option
    }

    let eoCalculationSetToMap eoCalculationSet =
        eoCalculationSet
        |> Set.map (fun (eoCalculation) -> eoCalculation.name, eoCalculation)
        |> Map.ofSeq

    type CalculatedEngageableOpponents = uint

    type EngageableOpponents =
        | Calculation of EngageableOpponentsCalculation
        | Calculated of CalculatedEngageableOpponents

    let determineEngageableOpponents numDice engageableOpponents =
        match engageableOpponents with
        | Calculated calculatedEngageableOpponents -> calculatedEngageableOpponents
        | Calculation eoCalculation ->
            divideUintsThenCompareToMaxThenRound numDice eoCalculation.combatRollDivisor eoCalculation.maxEOOption true

    //Todo: this needs to only parse uints
    let parseMaxEngageableOpponentsString input =
        if isNumeric input then Some(uint input) else None

    let parseEngaeableOpponentsString eoCalculationMap input =
        if isNumeric input then
            uint input |> Calculated
        elif Map.containsKey input eoCalculationMap then
            eoCalculationMap.Item input |> Calculation
        else
            Calculated 0u

module ZeroToFive =

    type ZeroToFive =
        | Zero
        | One
        | Two
        | Three
        | Four
        | Five

    let intToZeroToFiveOption num =
        match num with
        | 0 -> Some Zero
        | 1 -> Some One
        | 2 -> Some Two
        | 3 -> Some Three
        | 4 -> Some Four
        | 5 -> Some Five
        | _ -> None

    let zeroToFiveToUint zeroToFour =
        match zeroToFour with
        | Zero -> 0u
        | One -> 1u
        | Two -> 2u
        | Three -> 3u
        | Four -> 4u
        | Five -> 5u

    let zeroToFiveToFloat = zeroToFiveToUint >> float

    let zeroToFiveToInt = zeroToFiveToUint >> int

module Neg1To5 =
    open ZeroToFive

    type Neg1To5 =
        | NegOne
        | Zero
        | One
        | Two
        | Three
        | Four
        | Five

    let init () = Zero

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

    let neg1To5ToFloat = neg1To5ToInt >> float

    let zeroToFiveToNeg1To5 zeroToFive =
        match zeroToFive with
        | ZeroToFive.Zero -> Zero
        | ZeroToFive.One -> One
        | ZeroToFive.Two -> Two
        | ZeroToFive.Three -> Three
        | ZeroToFive.Four -> Four
        | ZeroToFive.Five -> Five

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

module DicePool =
    open ParsingUtils

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

    let base3d6DicePool = { emptyDicePool with d6 = 3u }

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
        |> String.concat commaAndSpace
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
    open ParsingUtils
    open DicePool

    type DicePoolPenalty = uint // always should deduct the dice with the fewest faces first (i.e. d4, then d6, then d8...)

    type DicePoolMod =
        | AddDice of DicePool
        | RemoveDice of DicePoolPenalty

    let uintToD6DicePoolMod (numDice: uint) =
        AddDice { emptyDicePool with d6 = numDice }

    let emptyDicePoolMod = uintToD6DicePoolMod 0u

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
        let d20, remainingNeg = removeDice dicePool.d20 d20Neg

        ({
            d4 = d4
            d6 = d6
            d8 = d8
            d10 = d10
            d12 = d12
            d20 = d20
         },
         remainingNeg)

    let modifyDicePool (dicePool: DicePool) (dicePoolMod: DicePoolMod) : DicePool =
        match dicePoolMod with
        | AddDice diceToAdd -> combineDicePools [ dicePool; diceToAdd ]
        | RemoveDice diceToRemove -> removeDiceFromDicePool dicePool diceToRemove |> (fun (dicePool, _) -> dicePool)

    let combineDicePoolModList dicePoolModList =

        let combinedPositiveDicePool =
            List.fold
                (fun acc diceMod ->
                    match diceMod with
                    | AddDice dicePool -> combineDicePools [ acc; dicePool ]
                    | _ -> acc)
                emptyDicePool
                dicePoolModList

        let combinedDicePoolPenalty =
            List.fold
                (fun acc diceMod ->
                    match diceMod with
                    | RemoveDice dicePoolPenalty -> acc + dicePoolPenalty
                    | _ -> acc)
                0u
                dicePoolModList

        removeDiceFromDicePool combinedPositiveDicePool combinedDicePoolPenalty
        |> (fun (dicePool, remainingNeg) ->
            if remainingNeg > 0u then
                RemoveDice remainingNeg
            else
                AddDice dicePool)

    let dicePoolModListToDicePool =
        combineDicePoolModList >> modifyDicePool emptyDicePool

    let dicePoolModListToNumDice = dicePoolModListToDicePool >> dicePoolToNumDice

    let dicePoolModToString dicePoolMod =
        match dicePoolMod with
        | AddDice dicePool -> "+" + dicePoolToString dicePool
        | RemoveDice dicePoolPenalty -> "-" + string dicePoolPenalty

    let dicePoolModListToString = combineDicePoolModList >> dicePoolModToString

    let modifyDicePoolByDicePoolModList dicePool dicePoolMods =

        dicePool
        |> AddDice
        |> List.singleton
        |> List.append dicePoolMods
        |> dicePoolModListToDicePool

    let intToD6DicePoolMod (num: int) =
        if num < 0 then
            RemoveDice(uint (abs num))
        else
            uintToD6DicePoolMod (uint num)

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
        dicePoolString
        |> commaSeperatedStringToList
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

module Feet =
    type Feet = private Feet of float

    // function used to extract data since type is private
    // let value (String50 str) = str
    let create feet =
        if feet >= 0 then Some(Feet feet) else None

module BattleMapUOM =
    open Feet
    let feetPerBattleMapUOM = 5u

module Range =

    open System
    open MathUtils

    type CalculatedRange = {
        name: string
        effectiveRange: uint
        maxRangeOption: uint option
    }

    type RangeCalculation = {
        name: string
        numDicePerEffectiveRangeUnit: uint
        ftPerEffectiveRangeUnit: uint
        roundEffectiveRangeUp: bool // If true, round up the effective range if decimal in calculation, otherwise round down
        maxRangeOption: uint option
    }

    type Range =
        | CalculatedRange of CalculatedRange
        | RangeCalculation of RangeCalculation

    //type RangeAdjustment = int

    let calculatedRangeToString (calculatedRange: CalculatedRange) =
        match calculatedRange.maxRangeOption with
        | Some maxRange -> sprintf "%d" maxRange
        | None -> ""
        |> sprintf "%d/%s" calculatedRange.effectiveRange

    let calculateRangeCalculation numDice rangeCalculation = {
        name = rangeCalculation.name
        effectiveRange =
            if rangeCalculation.roundEffectiveRangeUp then
                float numDice / float rangeCalculation.numDicePerEffectiveRangeUnit
                |> Math.Ceiling
                |> uint
                |> (*) rangeCalculation.ftPerEffectiveRangeUnit
            else
                float numDice / float rangeCalculation.numDicePerEffectiveRangeUnit
                |> Math.Floor
                |> uint
                |> (*) rangeCalculation.ftPerEffectiveRangeUnit
        maxRangeOption = rangeCalculation.maxRangeOption
    }

    let rangeToCalculatedRange (numDice: uint) (range: Range) : CalculatedRange =
        match range with
        | CalculatedRange calculatedRange -> calculatedRange
        | RangeCalculation rangeCalculation -> calculateRangeCalculation numDice rangeCalculation

    let determineGreatestRange numDice (primaryRange: Range) (optionalRange: Range option) =
        let calculatedPrimaryRange = rangeToCalculatedRange numDice primaryRange

        match optionalRange with
        | Some secondaryRange ->
            let calculatedSecondaryRange = rangeToCalculatedRange numDice secondaryRange

            if calculatedPrimaryRange.effectiveRange >= calculatedSecondaryRange.effectiveRange then
                calculatedPrimaryRange
            else
                calculatedSecondaryRange
        | None -> calculatedPrimaryRange

    let calculatedRangeListToRangeMap calculatedRangeList =
        List.map
            (fun (calculatedRange: CalculatedRange) -> calculatedRange.name, CalculatedRange calculatedRange)
            calculatedRangeList
        |> Map.ofList

    let rangeCalculationListToRangeMap rangeCalculationList =
        rangeCalculationList
        |> List.map (fun (rangeCalculation: RangeCalculation) ->
            rangeCalculation.name, RangeCalculation rangeCalculation)
        |> Map.ofList

    let createRangeMap calculatedRanges rangeCalculations : Map<string, Range> =
        Map.fold
            (fun acc key value -> Map.add key value acc)
            (calculatedRangeListToRangeMap calculatedRanges)
            (rangeCalculationListToRangeMap rangeCalculations)

    let isMeleeOrReachRange range =
        match range with
        | CalculatedRange calculatedRange ->
            match calculatedRange.name with
            | "Melee"
            | "Reach" -> true
            | _ -> false
        | _ -> false

module AreaOfEffectCalculation =

    type SphereCalculation = {
        name: string
        initRadius: float
        radiusPerDice: float
    }

    type ConeCalculation = {
        name: string
        initBaseAndHeight: float
        baseAndHeightPerDice: float
        angle: float
    }

    type AreaOfEffectCalculation =
        | SphereCalculation of SphereCalculation
        | ConeCalculation of ConeCalculation

module SetAreaOfEffect =

    type SetCone = {
        name: string
        baseAndHeight: uint
        angle: float
    }

    type SetSphere = { name: string; radius: uint }

    type SetAreaOfEffect =
        | SetCone of SetCone
        | SetSphere of SetSphere

    let setConeToString decimalPlaces (setCone: SetCone) =
        // let decimalLimitedArea =
        //     calculatedCone.area.ToString("F" + decimalPlaces.ToString())

        let decimalLimitedAngle = setCone.angle.ToString("F" + decimalPlaces.ToString())

        sprintf
            //"area: %s ft^2, distance: %u ft, angle: %s θ"
            //decimalLimitedArea
            "distance: %u ft, angle: %s θ"
            setCone.baseAndHeight
            decimalLimitedAngle

    let setSphereToString decimalPlaces calculatedSphere =
        // let decimalLimitedArea =
        //     calculatedSphere.area.ToString("F" + decimalPlaces.ToString())

        let decimalLimitedRadius =
            calculatedSphere.radius.ToString("F" + decimalPlaces.ToString())

        sprintf
            // "area: %s ft^2, radius: %s ft"
            // decimalLimitedArea
            "radius: %s ft"
            decimalLimitedRadius

    let setAreaOfEffectToString (setAOE: SetAreaOfEffect) =
        let decimalPlaces = 1

        match setAOE with
        | SetCone setCone -> setConeToString decimalPlaces setCone
        | SetSphere sphereShape -> setSphereToString decimalPlaces sphereShape

    let setAreaOfEffectOptionToString shapeOption =
        match shapeOption with
        | Some shape -> setAreaOfEffectToString shape
        | None -> ""

module AreaOfEffect =
    open System
    open MathUtils
    open BattleMapUOM
    open AreaOfEffectCalculation
    open SetAreaOfEffect

    type AreaOfEffect =
        | SetAreaOfEffect of SetAreaOfEffect
        | AreaOfEffectCalculation of AreaOfEffectCalculation

    let calcConeArea (distance: uint) (angle: float) : float =
        float (distance * distance) * Math.Tan(angle / 2.0)

    let calcConeDistance (area: uint) (angle: float) =
        uint (Math.Sqrt(float area / Math.Tan(angle / 2.)))

    let calcConeAngle (area: uint) (distance: uint) =
        2. * Math.Atan(Math.Sqrt(float area / float (distance * distance)))

    let coneCalculationToSetCone (coneCalculation: ConeCalculation) (numDice: uint) =

        {
            name = coneCalculation.name
            baseAndHeight =
                coneCalculation.initBaseAndHeight
                + (coneCalculation.baseAndHeightPerDice * (float numDice))
                |> roundDownToNearestMultipleOf5
            angle = coneCalculation.angle
        }

    let sphereCalculationToSetSphere (sphereCalculation: SphereCalculation) (numDice: uint) : SetSphere = {
        name = sphereCalculation.name
        radius =
            sphereCalculation.initRadius + (sphereCalculation.radiusPerDice * float numDice)
            |> roundDownToNearestMultipleOf5
    }

    let areaOfEffectCalculationToSetAreaOfEffect areaOfEffectCalculation numDice =
        match areaOfEffectCalculation with
        | ConeCalculation coneCalculation -> coneCalculationToSetCone coneCalculation numDice |> SetCone
        | SphereCalculation sphereCalculation -> sphereCalculationToSetSphere sphereCalculation numDice |> SetSphere

    let areaOfEffectToSetAreaOfEffect (aoe: AreaOfEffect) (numDice: uint) : SetAreaOfEffect =
        match aoe with
        | AreaOfEffectCalculation areaOfEffectCalculation ->
            areaOfEffectCalculationToSetAreaOfEffect areaOfEffectCalculation numDice
        | SetAreaOfEffect setAreaOfEffect -> setAreaOfEffect

    let determineAOEOption aoe numDice =
        match aoe with
        | Some aoe -> areaOfEffectToSetAreaOfEffect aoe numDice |> Some
        | None -> None

    let compareAndDetermineAOEShapeOption
        (numDice: uint)
        (aoe: AreaOfEffect option)
        (resourceAOE: AreaOfEffect option)
        : SetAreaOfEffect option =

        match resourceAOE with
        | Some resourceAOE -> Some(areaOfEffectToSetAreaOfEffect resourceAOE numDice)
        | None -> determineAOEOption aoe numDice

module SkillName =
    type SkillName = string

// Item Building

module ResourceName =
    type ResourceName = string

module WeaponResource =

    open DicePoolMod
    open Range
    open DamageType
    open AreaOfEffect
    open ResourceName
    open Penetration

    type WeaponResource = {
        name: string
        resourceName: ResourceName
        dicePoolMod: DicePoolMod
        penetration: Penetration
        rangeOption: Range option
        damageTypeSet: DamageType Set
        NamedAreaOfEffectOption: AreaOfEffect option
    }

    let weaponResourceToName weaponResource = weaponResource.name

module Weapon =
    open DicePoolMod
    open Range
    open DamageType
    open EngageableOpponents
    open AreaOfEffect
    open Penetration
    open ResourceName
    open SkillName

    type Weapon = {
        name: string
        governingSkillName: SkillName
        oneHandedDiceMod: DicePoolMod option
        twoHandedDiceMod: DicePoolMod option
        penetration: Penetration
        range: Range
        damageTypes: DamageType Set
        engageableOpponents: EngageableOpponents
        dualWieldedDiceMod: DicePoolMod option
        areaOfEffectOption: AreaOfEffect option
        resourceNameOption: ResourceName option
    }

module Container =
    type Container = {
        name: string
        weightCapacity: float
        volumeFtCubed: float
    }

module WeaponSpell =

    open DicePoolMod
    open Penetration
    open Range
    open EngageableOpponents
    open AreaOfEffect

    type WeaponSpell = {
        name: string
        oneHandedDiceMod: DicePoolMod option
        twoHandedDiceMod: DicePoolMod option
        dualWieldedDiceMod: DicePoolMod option
        penetration: Penetration
        range: Range
        engageableOpponents: EngageableOpponents
        areaOfEffectOption: AreaOfEffect option
        magicResourceAmount: uint
    }

// Character Building Blocks

module AttributeName =
    type AttributeName = string

    let toggleAttributeNameSet oldGoverningAttributeNames newGoverningAttributeName =
        oldGoverningAttributeNames
        |> Set.exists (fun attributeName -> attributeName = newGoverningAttributeName)
        |> (fun attributeNameExists ->
            if attributeNameExists then
                Set.remove newGoverningAttributeName oldGoverningAttributeNames
            else
                Set.add newGoverningAttributeName oldGoverningAttributeNames)

module Attribute =
    open Neg2To5
    open AttributeName

    type Attribute = {
        attributeName: AttributeName
        level: Neg2To5
    }

    let attributesToAttributeNames attributes =
        Seq.map (fun attriubte -> attriubte.attributeName) attributes

    let findAttributeWithAttributeName (set: Attribute Set) name =
        set |> Seq.find (fun attribute -> attribute.attributeName = name)

    let filterAttributeWithAttributeName attributeSet attributeName =
        Set.filter (fun attribute -> attribute.attributeName = attributeName) attributeSet

    let collectAttributesWithAttributeNames attributeSet attributeNameSet =
        Set.fold
            (fun acc attributeName -> attributeName |> filterAttributeWithAttributeName attributeSet |> Set.union acc)
            Set.empty
            attributeNameSet

    open Neg2To5
    open DicePoolMod

    let sumAttributesLevels (attributeNameList: AttributeName Set) (attributeList: Attribute Set) =
        attributeList
        |> List.ofSeq
        |> List.map (fun attribute ->
            if Set.contains attribute.attributeName attributeNameList then
                neg2To5ToInt attribute.level
            else
                0)
        |> List.sum

    let sumGoverningAttributeD6DiceMods attributeSet governingAttributeNameSet =
        sumAttributesLevels governingAttributeNameSet attributeSet |> intToD6DicePoolMod

module CoreSkillData =
    open AttributeName
    open SkillName

    type CoreSkillData = {
        skillName: SkillName
        attributeName: AttributeName
    }

module SpeedCalculation =
    open MathUtils

    type SpeedCalculation = {
        name: string
        feetPerGoverningSkillDice: float
        feetPerReactionSpeedAttribute: float
    }

    let calculateSpeed (numGoverningSkillDice: uint) (reactionSPeedAttributeInt: int) speed =
        (float numGoverningSkillDice * speed.feetPerGoverningSkillDice)
        |> (+) (float reactionSPeedAttributeInt * speed.feetPerReactionSpeedAttribute)
        |> roundDownToNearestMultipleOf5

module CombatSpeedCalculation =
    open SkillName
    open AttributeName
    open SpeedCalculation

    type CombatSpeedCalculation = {
        name: string
        governingSkillName: SkillName
        reactionSpeedAttributeName: AttributeName
        speed: SpeedCalculation
    }

    let combatSpeedCalculationToDescription combatSpeedCalculation =
        sprintf
            "+%.1f ft (per %s Dice), +/-%.1f (per %s)"
            combatSpeedCalculation.speed.feetPerGoverningSkillDice
            combatSpeedCalculation.governingSkillName
            combatSpeedCalculation.speed.feetPerReactionSpeedAttribute
            combatSpeedCalculation.reactionSpeedAttributeName

module CombatSpeed =
    open CombatSpeedCalculation

    type CombatSpeed = {
        calculatedSpeed: uint
        combatSpeedCalculation: CombatSpeedCalculation
        description: string
    }

// Effects

module DurationAndSource =
    type DurationAndSource = { duration: string; source: string }

module AttributeDeterminedDiceMod =
    open DurationAndSource
    open AttributeName
    open DicePoolMod

    type AttributeDeterminedDiceMod = {
        name: string
        attributesToEffect: AttributeName Set
        dicePoolMod: DicePoolMod
        durationAndSource: DurationAndSource
    }

    let attributeDeterminedDiceModsToDicePoolMods
        (governingAttributesOfSkill: AttributeName Set)
        attributeDeterminedDiceMods
        =
        attributeDeterminedDiceMods
        |> List.filter (fun attributeDeterminedDiceMod ->
            attributeDeterminedDiceMod.attributesToEffect
            |> Set.exists (fun attributeName -> Set.contains attributeName governingAttributesOfSkill))
        |> List.map (fun attributeDeterminedDiceMod -> attributeDeterminedDiceMod.dicePoolMod)

module PhysicalDefense =

    open DurationAndSource

    type PhysicalDefense = {
        name: string
        physicalDefense: float
        durationAndSource: DurationAndSource
    }

module SkillDiceMod =
    open DurationAndSource
    open DicePoolMod

    type SkillDiceMod = {
        name: string
        skillToEffect: string
        diceMod: DicePoolMod
        durationAndSource: DurationAndSource
    }

    let skillNameToSkillDiceMods skillName skillAdjustmentList =
        skillAdjustmentList
        |> List.filter (fun skillAdjustment -> skillAdjustment.skillToEffect = skillName)
        |> List.map (fun skillAdjustment -> skillAdjustment.diceMod)

module AttributeStatAdjustment =
    open DurationAndSource
    open AttributeName

    type AttributeStatAdjustment = {
        name: string
        attribute: AttributeName
        adjustment: int
        durationAndSource: DurationAndSource
    }

    let attributeStatAdjustmentToName attributeStatAdjustment = attributeStatAdjustment.name

    let attributeStatAdjustmentToEffectString attributeStatAdjustment =
        $"{attributeStatAdjustment.adjustment} {attributeStatAdjustment.attribute}"

module BaseDiceTier =

    open DicePool

    type BaseDiceTier = {
        itemPrefix: string
        level: int
        baseDice: DicePool
    }

module BaseDiceMod =
    open SkillName
    open DurationAndSource
    open DicePool
    open BaseDiceTier

    type BaseDiceMod = {
        name: string
        effectedSkillName: SkillName
        baseDiceTier: BaseDiceTier
        durationAndSource: DurationAndSource
    }

    let tryFindBaseDiceMod skillName baseDiceModList =
        baseDiceModList
        |> List.sortByDescending (fun baseDiceMod -> baseDiceMod.baseDiceTier.level)
        |> List.tryFind (fun baseDiceEffect -> baseDiceEffect.effectedSkillName = skillName)

    let findBaseDiceWith3d6Default skillName baseDiceModList =
        tryFindBaseDiceMod skillName baseDiceModList
        |> function
            | Some baseDiceMod -> baseDiceMod.baseDiceTier.baseDice
            | None -> base3d6DicePool

module TextEffect =
    open StringUtils
    open DurationAndSource
    open DicePoolMod

    open AttributeDeterminedDiceMod
    open PhysicalDefense
    open SkillDiceMod

    type TextEffect = {
        name: string
        effect: string
        durationAndSource: DurationAndSource
    }

    // AttributeDeterminedDiceMod
    let attributeDeterminedDiceModToEffectString (addm: AttributeDeterminedDiceMod) =
        sprintf
            "%s to %s"
            (dicePoolModToString addm.dicePoolMod)
            (stringSeqToStringSeperatedByCommaAndSpace addm.attributesToEffect)

    let attributeDeterminedDiceModToTextEffect (addm: AttributeDeterminedDiceMod) = {
        name = addm.name
        effect = attributeDeterminedDiceModToEffectString addm
        durationAndSource = addm.durationAndSource
    }

    // PhysicalDefense
    let physicalDefenseToEffectString pd =
        sprintf "+%.2f to Physical Defense" pd.physicalDefense

    let physicalDefenseToNameAndEffect (pd: PhysicalDefense) = {
        name = pd.name
        effect = physicalDefenseToEffectString pd
        durationAndSource = pd.durationAndSource
    }

    // SkillDiceMod
    let skillDiceModToEffectString sdm =
        sprintf "%s to %s" (dicePoolModToString sdm.diceMod) sdm.skillToEffect

    let skillDiceModToTextEffect (sdm: SkillDiceMod) = {
        name = sdm.name
        effect = skillDiceModToEffectString sdm
        durationAndSource = sdm.durationAndSource
    }

module Effect =
    open StringUtils
    open SkillDiceMod
    open AttributeStatAdjustment
    open PhysicalDefense
    open AttributeDeterminedDiceMod
    open Weapon
    open WeaponResource
    open BaseDiceMod
    open TextEffect

    type Effect =
        | Weapon of Weapon
        | WeaponResource of WeaponResource
        | SkillDiceMod of SkillDiceMod
        | AttributeStatAdjustment of AttributeStatAdjustment
        | PhysicalDefense of PhysicalDefense
        | AttributeDeterminedDiceMod of AttributeDeterminedDiceMod
        | BaseDiceMod of BaseDiceMod
        | TextEffect of TextEffect

    let effectToEffectName effect =
        match effect with
        | Weapon weapon -> weapon.name
        | WeaponResource weaponResource -> weaponResource.name
        | SkillDiceMod skillDiceModEffect -> skillDiceModEffect.name
        | AttributeStatAdjustment attributeStatAdjustment -> attributeStatAdjustment.name
        | PhysicalDefense defenseClass -> defenseClass.name
        | AttributeDeterminedDiceMod addme -> addme.name
        //| MovementSpeedCalculation msc -> msc.name
        | BaseDiceMod baseDiceEffect -> baseDiceEffect.name
        | TextEffect textEffect -> textEffect.name

    let effectsToCommaSeperatedEffectNames effects =
        effects
        |> Set.map effectToEffectName
        |> stringSeqToStringSeperatedByCommaAndSpace

    let effectToSkillDiceModEffectList (effect: Effect) =
        match effect with
        | SkillDiceMod skillAdjustment -> Some skillAdjustment
        | _ -> None

    let effectsToSkillDiceModEffectList = List.choose effectToSkillDiceModEffectList

    let effectToAttributeDeterminedDiceModEffectList (effect: Effect) =
        match effect with
        | AttributeDeterminedDiceMod addme -> Some addme
        | _ -> None

    let effectsToAttributeDeterminedDiceModEffectList =
        List.choose effectToAttributeDeterminedDiceModEffectList

    let effectToBaseDiceMod effect =
        match effect with
        | BaseDiceMod bdm -> Some bdm
        | _ -> None

    let effectsToBaseDiceModList = List.choose effectToBaseDiceMod

    let effectToWeaponResourceOption effect =
        match effect with
        | WeaponResource weaponResource -> Some weaponResource
        | _ -> None

    let effectsToWeaponResourceList = List.choose effectToWeaponResourceOption

    let effectToWeaponOption effect =
        match effect with
        | Weapon weapon -> Some weapon
        | _ -> None

    let effectsToWeaponList = List.choose effectToWeaponOption

    let effectToNonBaseDiceModEffects effects =
        effects
        |> List.filter (fun effect ->
            match effect with
            | BaseDiceMod _ -> false
            | _ -> true)

    let effectToTextEffect effect =
        match effect with
        | TextEffect te -> te
        | SkillDiceMod sdm -> skillDiceModToTextEffect sdm
        | AttributeDeterminedDiceMod addm -> attributeDeterminedDiceModToTextEffect addm
        | PhysicalDefense pd -> physicalDefenseToNameAndEffect pd

// Item

module Item =
    open Effect

    type Item = {
        name: string
        itemEffectSet: Effect Set
        value: string
        weight: float
    }

    let sumItemListWeight itemList =
        if List.isEmpty itemList then
            0.0
        else
            List.sumBy (fun item -> item.weight) itemList

    let itemToEffectList item = item.itemEffectSet |> List.ofSeq

module ItemStack =
    open Item
    open Effect

    type ItemStack = { item: Item; quantity: uint }

    let sumItemStackWeight itemStack =
        itemStack.item.weight * (float itemStack.quantity)

    let sumItemStackListWeight (itemStackList: ItemStack list) =
        itemStackList |> List.map sumItemStackWeight |> List.sum

    let itemStackToEffectList (itemStack: ItemStack) = itemToEffectList itemStack.item

// ItemStat

module ItemElement =

    open Item
    open ItemStack
    open Container
    open Effect

    type ContainerItem = {
        item: Item
        containerTypeData: Container
        containedElements: ItemElement list
    }

    and ItemElement =
        | Item of Item
        | ContainerItem of ContainerItem
        | ItemStack of ItemStack

    let containerItemToEffects (containerItem: ContainerItem) =
        containerItem.item.itemEffectSet |> Set.toList

    let itemElementToItem itemElement =
        match itemElement with
        | Item item -> item
        | ContainerItem containerItem -> containerItem.item
        | ItemStack itemStack -> itemStack.item

    let itemElementToName itemElement =
        let temp = itemElementToItem itemElement
        temp.name

    let rec getItemElementWeight itemElement =
        match itemElement with
        | Item item -> item.weight
        | ContainerItem containerItem ->
            containerItem.item.weight
            + (containerItem.containedElements |> List.map getItemElementWeight |> List.sum)
        | ItemStack itemStack -> sumItemStackWeight itemStack

    let sumItemElementListWeight itemElementList =
        itemElementList |> List.map getItemElementWeight |> List.sum

    let itemElementToNonContainedEffects itemElement =
        match itemElement with
        | Item item -> itemToEffectList item
        | ContainerItem containerItem -> containerItemToEffects containerItem
        | ItemStack itemStack -> itemStackToEffectList itemStack

    let itemElementListToNonContainedEffects itemElementList =
        List.collect itemElementToNonContainedEffects itemElementList

    let itemElementToWeaponList =
        itemElementToNonContainedEffects >> effectsToWeaponList

    let itemElementToBaseDiceEffectList =
        itemElementToNonContainedEffects >> effectsToBaseDiceModList

    // List stuff

    let itemElementListToEffectList = List.collect itemElementToNonContainedEffects

    let itemElementListToNonDuplicateWeaponResourceList itemStackList =
        itemStackList
        |> itemElementListToEffectList
        |> effectsToWeaponResourceList
        |> List.distinct

module DicePoolCalculation =
    open Attribute
    open DicePoolMod
    open BaseDiceMod
    open Neg1To5
    open ZeroToFive
    open Effect
    open AttributeDeterminedDiceMod
    open SkillDiceMod

    type DicePoolCalculationData = {
        attributes: Attribute Set
        effects: Effect List
    }

    let createDicePoolModList skillName levelDiceMod governingAttributeNameSet dicePoolCalculationData =
        let baseDice =
            dicePoolCalculationData.effects
            |> effectsToBaseDiceModList
            |> findBaseDiceWith3d6Default skillName

        [
            [ baseDice |> AddDice ]
            [ levelDiceMod ]
            [
                governingAttributeNameSet
                |> sumGoverningAttributeD6DiceMods dicePoolCalculationData.attributes
            ]
            (dicePoolCalculationData.effects
             |> effectsToSkillDiceModEffectList
             |> skillNameToSkillDiceMods skillName)
            (dicePoolCalculationData.effects
             |> effectsToAttributeDeterminedDiceModEffectList
             |> attributeDeterminedDiceModsToDicePoolMods governingAttributeNameSet)
        ]
        |> List.collect id

    let createSkillDicePoolMods
        skillName
        skillLevel
        governingAttributeNameSet
        (dicePoolCalculationData: DicePoolCalculationData)
        =
        let skillLevelDiceMod = skillLevel |> neg1To5ToInt |> intToD6DicePoolMod
        createDicePoolModList skillName skillLevelDiceMod governingAttributeNameSet dicePoolCalculationData

    let calculateSkillDicePool
        skillName
        skillLevel
        governingAttributeNameSet
        (dicePoolCalculationData: DicePoolCalculationData)
        =
        createSkillDicePoolMods skillName skillLevel governingAttributeNameSet dicePoolCalculationData
        |> dicePoolModListToDicePool

    let calculateVocationStatDicePool
        vocationStatName
        vocationStatLevel
        governingAttributeNameSet
        dicePoolCalculationData
        =
        let vocationStatDiceMod = vocationStatLevel |> zeroToFiveToInt |> intToD6DicePoolMod

        createDicePoolModList vocationStatName vocationStatDiceMod governingAttributeNameSet dicePoolCalculationData
        |> dicePoolModListToDicePool

module Skill =
    open Neg1To5
    open DicePoolMod
    open SkillName
    open DicePoolCalculation
    open AttributeName

    type Skill = {
        name: SkillName
        level: Neg1To5
        governingAttributeNames: AttributeName Set
        dicePoolModList: DicePoolMod List
    }

    let init name level governingAttributes dicePoolCalculationData =

        {
            name = name
            level = level
            governingAttributeNames = governingAttributes
            dicePoolModList = createSkillDicePoolMods name level governingAttributes dicePoolCalculationData
        }

module WeaponSkillData =
    open AttributeName

    type WeaponSkillData = {
        name: string
        governingAttributes: AttributeName Set
    }

// Larger Character Building Blocks

module VocationStat =
    open ZeroToFive
    open DicePool
    open AttributeName

    type VocationStat = {
        name: string
        governingAttributeNameSet: AttributeName Set
        level: ZeroToFive
        dicePool: DicePool
    }

module MundaneVocationSkill =
    open Skill

    type MundaneVocationSkill =
        | VocationalSkill of Skill
        | WeaponSkill of Skill

    let mundaneVocationSkillToSkill mundaneVocationSkill =
        match mundaneVocationSkill with
        | VocationalSkill skill -> skill
        | WeaponSkill skill -> skill

    let mundaneVocationSkillsToSkills mundaneVocationSkills =
        Set.map mundaneVocationSkillToSkill mundaneVocationSkills

    let mundaneVocationSkillToWeaponSkillOption =
        function
        | VocationalSkill _ -> None
        | WeaponSkill skill -> Some skill

    let mundaneVocationSkillsToWeaponSkills seq =
        Seq.choose mundaneVocationSkillToWeaponSkillOption seq

module MundaneVocation =
    open VocationStat
    open MundaneVocationSkill

    type MundaneVocation = {
        vocationStat: VocationStat
        mundaneVocationSkills: MundaneVocationSkill Set
    }

// Magic

module MagicResourcePool =

    open Neg1To5
    open ZeroToFive
    open System

    let determineVocationLevelFloatForCalc vocationLevel =

        match vocationLevel with
        | Zero -> 0.5
        | positiveLevel -> zeroToFiveToFloat positiveLevel

    let calculateVocationMagicResource (vocationLevel: ZeroToFive) (vocationDicePoolSize: uint) =
        vocationDicePoolSize
        |> float
        |> (*) (determineVocationLevelFloatForCalc vocationLevel)
        |> Math.Floor
        |> uint

    let determineGoverningCoreSkillLevelFloatForCalc governingCoreSkillLevel =
        match governingCoreSkillLevel with
        | Neg1To5.NegOne -> 0.0
        | Neg1To5.Zero -> 0.5
        | positiveLevel -> neg1To5ToFloat positiveLevel

    let calcGoverningSkillMagicResource (governingCoreSkillLevel: Neg1To5) (governingCoreSkillDicePoolSize: uint) =
        governingCoreSkillDicePoolSize
        |> float
        |> (*) (determineGoverningCoreSkillLevelFloatForCalc governingCoreSkillLevel)
        |> (*) 0.5
        |> Math.Floor
        |> uint

module MagicSkillData =
    open DamageType

    type MagicSkillData = {
        name: string
        damageTypes: DamageType Set
        isMeleeCapable: bool
        isRangeCapable: bool
    }

module MagicSystem =
    open AttributeName
    open MagicSkillData

    type MagicSystem = {
        name: string
        vocationName: string
        vocationGoverningAttributeSet: AttributeName Set
        resourceName: string
        governingCoreSkill: string
        magicSkillDataMap: Map<string, MagicSkillData>
    }

module MagicVocationSkill =
    open MundaneVocationSkill
    open Skill

    type MagicVocationSkill =
        | MagicSkill of Skill
        | MundaneVocationSkill of MundaneVocationSkill

    let magicVocationSkillToSkill magicVocationSkill =
        match magicVocationSkill with
        | MagicSkill skill -> skill
        | MundaneVocationSkill mundaneVocationSkill -> mundaneVocationSkillToSkill mundaneVocationSkill

    let magicVocationSkillsToSkills magicVocationSkills =
        Set.map magicVocationSkillToSkill magicVocationSkills

    let magicVocationSkillToWeaponSkillOption =
        function
        | MagicSkill _ -> None
        | MundaneVocationSkill mundaneVocationSkill -> mundaneVocationSkillToWeaponSkillOption mundaneVocationSkill

    let magicVocationSkillsToWeaponSkills magicVocationSkills : seq<Skill> =
        Seq.choose magicVocationSkillToWeaponSkillOption magicVocationSkills

    let magicVocationSKillToMagicSkill =
        function
        | MagicSkill skill -> Some skill
        | _ -> None

    let magicVocationSkillstoMagicSkills magicVocationSkills : seq<Skill> =
        Seq.choose magicVocationSKillToMagicSkill magicVocationSkills

module MagicVocationExtras =
    open MagicSystem
    open MagicVocationSkill

    type MagicVocationExtras = {
        magicVocationSkills: MagicVocationSkill Set
        magicSystem: MagicSystem
        vocationResourcePool: uint
        coreSkillResourcePool: uint
        currentMagicResource: uint
    }

    let magicVocationExtrasToWeaponSkills magicVocationExtras =
        magicVocationExtras.magicVocationSkills |> magicVocationSkillsToWeaponSkills

    let magicVocationExtrasToMagicSkills magicVocationExtras =
        magicVocationExtras.magicVocationSkills |> magicVocationSkillstoMagicSkills

    let magicVocationExtrasToMagicSkillsAndMagicSystem magicVocationExtras =
        (magicVocationExtrasToMagicSkills magicVocationExtras, magicVocationExtras.magicSystem)

module MundaneOrMagicVocationExtras =
    open MundaneVocationSkill
    open MagicVocationExtras

    type MundaneOrMagicVocationExtras =
        | MundaneVocationExtras of MundaneVocationSkill Set
        | MagicVocationExtras of MagicVocationExtras

module Vocation =
    open VocationStat
    open MundaneOrMagicVocationExtras

    open MagicVocationExtras
    open MundaneVocationSkill

    type Vocation = {
        vocationStat: VocationStat
        mundaneOrMagicVocationExtras: MundaneOrMagicVocationExtras
    }

    let vocationListToWeaponSkillList (vocationList: Vocation List) =
        vocationList
        |> List.collect (fun vocation ->
            match vocation.mundaneOrMagicVocationExtras with
            | MagicVocationExtras magicVocationExtras ->
                magicVocationExtrasToWeaponSkills magicVocationExtras |> List.ofSeq
            | MundaneVocationExtras mundaneVocationSkills ->
                mundaneVocationSkillsToWeaponSkills mundaneVocationSkills |> List.ofSeq)

    let vocationListToMagicSkillsAndMagicSystem vocations =
        vocations
        |> List.choose (fun vocation ->
            match vocation.mundaneOrMagicVocationExtras with
            | MagicVocationExtras(magicVocationExtras: MagicVocationExtras.MagicVocationExtras) ->
                magicVocationExtrasToMagicSkillsAndMagicSystem magicVocationExtras |> Some
            | MundaneVocationExtras _ -> None)

module CombatRoll =

    open DicePool
    open DicePoolMod
    open Penetration
    open Range
    open DamageType
    open SetAreaOfEffect
    open EngageableOpponents
    open Effect
    open DicePoolCalculation
    open Skill
    open BaseDiceMod
    open Weapon
    open ItemElement
    open WeaponResource
    open AreaOfEffect
    open WeaponSkillData
    open Vocation

    type CombatRoll = {
        itemName: string
        weaponTypeName: string
        handedVariation: string
        resourceName: string
        resourceDicePoolMod: DicePoolMod
        dicePool: DicePool
        weaponandOffhandDicePoolModString: DicePoolMod
        calculatedRange: CalculatedRange
        penetration: Penetration
        damageTypeSet: DamageType Set
        setAreaOfEffectOption: SetAreaOfEffect Option
        calculatedEngageableOpponents: CalculatedEngageableOpponents
        eoName: string option
    }

    let weaponResourceClassOptionToWeaponResourceClass resource =
        match resource with
        | Some(resource: WeaponResource) ->
            (resource.name,
             resource.dicePoolMod,
             resource.penetration,
             resource.rangeOption,
             resource.damageTypeSet,
             resource.NamedAreaOfEffectOption)
        | None -> ("", emptyDicePoolMod, 0u, None, Set.empty, None)

    let createCombatRoll
        (itemName: string)
        (weaponName: string)
        (weaponPenetration: Penetration)
        (weaponRange: Range)
        (weaponDamageTypeSet: DamageType Set)
        (weaponEO: EngageableOpponents)
        (weaponAOEOption: AreaOfEffect option)
        (skillDicePoolModList: DicePoolMod List)
        (resource: option<WeaponResource>)
        (handedVariationName: string)
        (weaponDiceMod: DicePoolMod)
        (offHandedWeaponDiceMod: DicePoolMod)
        : CombatRoll =

        let (resourceName, resourceDice, resourcePenetration, resourceRange, resourceDamageTypeSet, resourceAreaOfEffect) =
            weaponResourceClassOptionToWeaponResourceClass resource

        let dicePool =
            dicePoolModListToDicePool (skillDicePoolModList @ [ weaponDiceMod; offHandedWeaponDiceMod; resourceDice ])

        let numDice = dicePool |> dicePoolToNumDice

        {
            itemName = itemName
            resourceName = resourceName
            resourceDicePoolMod = resourceDice
            dicePool = dicePool
            weaponandOffhandDicePoolModString = combineDicePoolModList [ weaponDiceMod; offHandedWeaponDiceMod ]
            calculatedRange = determineGreatestRange numDice weaponRange resourceRange
            penetration = weaponPenetration + resourcePenetration
            damageTypeSet = Set.union weaponDamageTypeSet resourceDamageTypeSet
            setAreaOfEffectOption = compareAndDetermineAOEShapeOption numDice weaponAOEOption resourceAreaOfEffect
            calculatedEngageableOpponents = determineEngageableOpponents numDice weaponEO
            eoName =
                (match weaponEO with
                 | Calculation eoCalc -> Some eoCalc.name
                 | Calculated _ -> None)
            weaponTypeName = weaponName
            handedVariation = handedVariationName
        }

    let preloadedCreateWeaponCombatRoll
        itemName
        name
        penetration
        range
        damageTypes
        engageableOpponents
        areaOfEffectOption
        skillDicePoolModList
        itemResourceNameAndWeaponResourceOption
        : string -> DicePoolMod -> DicePoolMod -> CombatRoll =
        createCombatRoll
            itemName
            name
            penetration
            range
            damageTypes
            engageableOpponents
            areaOfEffectOption
            skillDicePoolModList
            itemResourceNameAndWeaponResourceOption

    let createHandedVariationCombatRolls
        (itemName: string)
        name
        oneHandedDiceMod
        twoHandedDiceMod
        dualWieldedDiceMod
        penetration
        range
        damageTypes
        engageableOpponents
        areaOfEffectOption
        (skillDicePoolModList: DicePoolMod List)
        itemResourceNameAndWeaponResourceOption
        : CombatRoll list =

        let preloadedCreateWeaponCombatRoll =
            createCombatRoll
                itemName
                name
                penetration
                range
                damageTypes
                engageableOpponents
                areaOfEffectOption
                skillDicePoolModList
                itemResourceNameAndWeaponResourceOption

        let oneHandedCombatRollOption =
            match oneHandedDiceMod with
            | Some oneHandedDiceMod ->
                preloadedCreateWeaponCombatRoll "One-Handed" oneHandedDiceMod emptyDicePoolMod
                |> Some
            | None -> None

        let twoHandedCombatRollOption =
            match twoHandedDiceMod with
            | Some twoHandedDiceMod ->
                preloadedCreateWeaponCombatRoll "Two-Handed" twoHandedDiceMod emptyDicePoolMod
                |> Some
            | None -> None

        let dualWieldedCombatRollOption =
            match oneHandedDiceMod, dualWieldedDiceMod with
            | Some weaponHandedDice, Some offHandedDicePoolMod ->
                preloadedCreateWeaponCombatRoll "Dual-Wielded" weaponHandedDice offHandedDicePoolMod
                |> Some
            | _, _ -> None

        [
            oneHandedCombatRollOption
            twoHandedCombatRollOption
            dualWieldedCombatRollOption
        ]
        |> List.choose id

    let createWeaponItemCombatRolls
        (equipmentList: ItemElement List)
        (weaponSkillList: Skill List)
        (weaponSkillDataMap: Map<string, WeaponSkillData>)
        (dicePoolCalculationData: DicePoolCalculationData)
        : CombatRoll List =

        equipmentList
        // Step 1: filter down to a tuple of item name and weapon
        |> List.collect (fun itemElement ->
            itemElement
            |> itemElementToWeaponList
            |> List.map (fun weapon ->
                let baseDiceModOption =
                    itemElement
                    |> itemElementToBaseDiceEffectList
                    |> tryFindBaseDiceMod weapon.governingSkillName

                // Filters out base Dice Mods that could be from other weapons
                let filteredDicePoolCalculationData = {
                    dicePoolCalculationData with
                        effects =
                            dicePoolCalculationData.effects
                            |> effectToNonBaseDiceModEffects
                            |> (fun effects ->
                                match baseDiceModOption with
                                | Some baseDiceMod -> List.append [ BaseDiceMod baseDiceMod ] effects
                                | None -> effects)
                }

                (itemElementToName itemElement, weapon, filteredDicePoolCalculationData)))
        // Step 2: Find weapons that have weapon Resources. If weapon has a weapon resource, but no weapon resource is in equipment then none, else save tuple with weapon resource tuple. If weapon has no weapon resource, than just return tuple with none for the weaponResource tuple.
        |> List.collect (fun (itemElementName, weapon, filteredDicePoolCalculationData) ->
            match weapon.resourceNameOption with
            | Some resourceClass ->
                equipmentList
                |> itemElementListToNonDuplicateWeaponResourceList
                |> List.choose (fun weaponResource ->
                    if weaponResource.resourceName = resourceClass then
                        Some(itemElementName, weapon, filteredDicePoolCalculationData, Some weaponResource)
                    else
                        None)
            | None -> List.singleton (itemElementName, weapon, filteredDicePoolCalculationData, None))

        // Step 3: Try to find the weaponSkill that governs the specific weapon. If none exists, init a "fake" skill at level 0.
        // If some weaponSkillData exists, check if they have a weapon skill for it already.
        // If so, return the skill, else make a "default" skill using weaponSkillData.
        |> List.collect (fun (itemName, weapon, filteredDicePoolCalculationData, weaponResourceOption) ->
            weapon.governingSkillName
            |> weaponSkillDataMap.TryFind
            |> function
                | None -> Skill.init weapon.governingSkillName Neg1To5.Zero Set.empty filteredDicePoolCalculationData
                | Some weaponSkillData ->
                    weaponSkillList
                    |> List.tryFind (fun skill -> skill.name = weaponSkillData.name)
                    |> function
                        | Some skill -> skill
                        | None ->
                            Skill.init
                                weaponSkillData.name
                                Neg1To5.Zero
                                weaponSkillData.governingAttributes
                                filteredDicePoolCalculationData
            |> (fun skill ->
                createHandedVariationCombatRolls
                    itemName
                    weapon.name
                    weapon.oneHandedDiceMod
                    weapon.twoHandedDiceMod
                    weapon.dualWieldedDiceMod
                    weapon.penetration
                    weapon.range
                    weapon.damageTypes
                    weapon.engageableOpponents
                    weapon.areaOfEffectOption
                    skill.dicePoolModList
                    weaponResourceOption))

    open WeaponSpell

    let createMagicCombatRolls (vocationList: Vocation List) (weaponSpellSet: WeaponSpell Set) =

        vocationList
        |> vocationListToMagicSkillsAndMagicSystem
        |> List.collect (fun (magicSkills, magicSystem: MagicSystem.MagicSystem) ->
            magicSkills
            |> Seq.map (fun magicSkill -> (magicSkill, magicSystem.magicSkillDataMap.Item magicSkill.name))
            |> Seq.toList)
        |> List.collect (fun (magicSkill, magicSkillData: MagicSkillData.MagicSkillData) ->
            weaponSpellSet
            |> Set.toSeq
            |> Seq.choose (fun weaponSpell ->
                if (isMeleeOrReachRange weaponSpell.range) && magicSkillData.isMeleeCapable then
                    Some(magicSkill, magicSkillData, weaponSpell)
                elif (not (isMeleeOrReachRange weaponSpell.range)) && magicSkillData.isRangeCapable then
                    Some(magicSkill, magicSkillData, weaponSpell)
                else
                    None)
            |> Seq.toList)
        |> List.collect (fun (magicSkill, magicSkillData, weaponSpell: WeaponSpell) ->
            createHandedVariationCombatRolls
                magicSkill.name
                weaponSpell.name
                weaponSpell.oneHandedDiceMod
                weaponSpell.twoHandedDiceMod
                weaponSpell.dualWieldedDiceMod
                weaponSpell.penetration
                weaponSpell.range
                magicSkillData.damageTypes
                weaponSpell.engageableOpponents
                weaponSpell.areaOfEffectOption
                magicSkill.dicePoolModList
                None)

    let createCombatRolls
        (equipmentList: ItemElement List)
        (vocationList: Vocation List)
        (weaponSkillDataMap: Map<string, WeaponSkillData>)
        (weaponSpellSet: WeaponSpell Set)
        (dicePoolCalculationData: DicePoolCalculationData)
        : CombatRoll List =

        let weaponCombatRolls =
            createWeaponItemCombatRolls
                equipmentList
                (vocationListToWeaponSkillList vocationList)
                weaponSkillDataMap
                dicePoolCalculationData

        let magicCombatRolls = createMagicCombatRolls vocationList weaponSpellSet

        List.collect id [ weaponCombatRolls; magicCombatRolls ]

module CharacterInformation =
    type CharacterInformation = {
        notes: string
        appearance: string
        disposition: string
        beliefsAndMorality: string
        goalsAndAspirations: string
        backstory: string
    }

module CarryWeightCalculation =
    open Attribute
    open Neg1To5
    open Skill
    open AttributeName

    type CarryWeightCalculation = {
        name: string
        baseWeight: uint
        governingAttribute: AttributeName
        weightIncreasePerAttribute: uint
        governingSkill: string
        weightIncreasePerSkill: uint
    }

    let calculateCarryWeight (carryWeightCalculation: CarryWeightCalculation) attributes coreSkills =

        let attributeLevel =
            sumAttributesLevels (Set.ofList [ carryWeightCalculation.governingAttribute ]) attributes

        let skillLevel =
            coreSkills
            |> Set.toList
            |> List.tryFind (fun (skill: Skill) -> skill.name = carryWeightCalculation.governingSkill)
            |> function
                | Some skill -> skill.level
                | None -> Zero
            |> neg1To5ToInt


        int carryWeightCalculation.baseWeight
        + (attributeLevel * int carryWeightCalculation.weightIncreasePerAttribute)
        + (skillLevel * int carryWeightCalculation.weightIncreasePerSkill)
        |> float

module WeightClass =
    open AttributeDeterminedDiceMod

    type WeightClass = {
        name: string
        bottomPercentOption: float option
        topPercentOption: float option
        attributeDeterminedDiceModEffect: AttributeDeterminedDiceMod
    }

    let floatPercentToUintPercent percentFloat = percentFloat * 100.0 |> uint

    let weightClassSourceString weightClass currentWeight maxWeight =

        let topPercent =
            match weightClass.topPercentOption with
            | Some topPercent ->
                topPercent
                |> floatPercentToUintPercent
                |> (fun topPercent -> $"""<= {topPercent}{"%"}""")
            | None -> ""

        let bottomPercent =
            match weightClass.bottomPercentOption with
            | Some bottomPercent ->
                bottomPercent
                |> floatPercentToUintPercent
                |> (fun bottomPercent -> $"""{bottomPercent}{"%"} <=""")
            | None -> ""


        $"""{currentWeight}/{maxWeight} ({bottomPercent} {"%"} of carry weight {topPercent})"""

    let weightClassOptionSourceString weightClassOption currentWeight maxWeight =
        match weightClassOption with
        | Some weightClass -> weightClassSourceString weightClass currentWeight maxWeight
        | None -> ""

    let determineWeightClass (maxCarryWeight: float) (inventoryWeight: float) (weightClassSet: WeightClass Set) =

        let percentOfMaxCarryWeight = inventoryWeight / maxCarryWeight

        let checkTopPercent topPercent = percentOfMaxCarryWeight <= topPercent

        let checkBottomPercent bottomPercent = bottomPercent < percentOfMaxCarryWeight

        weightClassSet
        |> List.ofSeq
        |> List.tryFind (fun weightClass ->
            match weightClass.bottomPercentOption, weightClass.topPercentOption with
            | None, Some topPercent ->
                // Generally used for setting the bottom of a weightClass range (e.i. Light)
                checkTopPercent topPercent
            | Some bottomPercent, Some topPercent ->
                // Generally used for setting the inbetween elements of a weightClass range (e.i. Medium, Heavy)
                (checkTopPercent topPercent) && (checkBottomPercent bottomPercent)
            | Some bottomPercent, None ->
                // Generally used for setting the upper bound of a weightClass range (e.i. Overencumbered)
                checkBottomPercent bottomPercent
            | None, None ->
                // Only if impossible case is defined that can never be used
                false)

module SettingData =

    open AttributeName
    open CoreSkillData
    open ItemElement
    open WeaponSpell
    open MagicSystem
    open WeaponSkillData
    open Effect
    open CombatSpeedCalculation
    open CarryWeightCalculation
    open WeightClass

    type SettingData = {
        attributeNameSet: AttributeName Set
        coreSkillDataSet: CoreSkillData Set
        itemElementMap: Map<string, ItemElement>
        weaponSpellSet: WeaponSpell Set
        magicSystemMap: Map<string, MagicSystem>
        weaponSkillDataMap: Map<string, WeaponSkillData>
        effectMap: Map<string, Effect>
        combatSpeedCalculationMap: Map<string, CombatSpeedCalculation>
        carryWeightCalculationMap: Map<string, CarryWeightCalculation>
        weightClassSet: WeightClass Set
    }

    let init () = {
        attributeNameSet = Set.empty
        coreSkillDataSet = Set.empty
        itemElementMap = Map.empty
        weaponSpellSet = Set.empty
        magicSystemMap = Map.empty
        weaponSkillDataMap = Map.empty
        effectMap = Map.empty
        combatSpeedCalculationMap = Map.empty
        carryWeightCalculationMap = Map.empty
        weightClassSet = Set.empty
    }

module ZeroToThree =
    type ZeroToThree =
        | Zero
        | One
        | Two
        | Three

    let zeroToThreeToUint zeroToThree =
        match zeroToThree with
        | Zero -> 0u
        | One -> 1u
        | Two -> 2u
        | Three -> 3u

module Character =
    open Attribute
    open Skill
    open ZeroToThree
    open Vocation
    open DicePoolCalculation
    open ItemElement
    open CombatRoll
    open CharacterInformation
    open Effect
    open CombatSpeed
    open SettingData
    open WeightClass
    open CarryWeightCalculation

    type Character = {
        name: string
        attributes: Attribute Set
        coreSkills: Skill Set
        destinyPoints: ZeroToThree
        vocationList: Vocation list
        equipment: ItemElement List
        combatRollList: CombatRoll List
        characterInformation: CharacterInformation
        characterEffects: Effect List
        combatSpeeds: CombatSpeed List
        settingData: SettingData
        weightClassOption: WeightClass option
        carryWeightCalculationOption: CarryWeightCalculation option
    }

    let characterToDicePoolCalculationDataWithoutWeightClassOptionEffect character = {
        effects =
            character.characterEffects
            @ itemElementListToNonContainedEffects character.equipment
        attributes = character.attributes
    }

    let characterToDicePoolCalculationData character =
        character
        |> characterToDicePoolCalculationDataWithoutWeightClassOptionEffect
        |> (fun dicePoolCalcuationData -> {
            dicePoolCalcuationData with
                effects =
                    dicePoolCalcuationData.effects
                    @ match character.weightClassOption with
                      | Some weightClass -> [
                          weightClass.attributeDeterminedDiceModEffect |> AttributeDeterminedDiceMod
                        ]
                      | None -> []
        })