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
        maxEO: uint option
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
            divideUintsThenCompareToMaxThenRound numDice eoCalculation.combatRollDivisor eoCalculation.maxEO true

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

    let dicePoolModToInt dicePoolMod =
        match dicePoolMod with
        | AddDice dicePool -> dicePoolToNumDice dicePool |> int
        | RemoveDice dicePoolPenalty -> int dicePoolPenalty * -1

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

    let weaponResourceClassOptionToWeaponResourceClass (resource: WeaponResource option) =
        match resource with
        | Some resource ->
            (" (" + resource.name + ")",
             resource.dicePoolMod,
             resource.penetration,
             resource.rangeOption,
             resource.damageTypeSet,
             resource.NamedAreaOfEffectOption)
        | None -> ("", uintToD6DicePoolMod (0u), 0u, None, Set.empty, None)

module Weapon =
    open DicePoolMod
    open Range
    open DamageType
    open EngageableOpponents
    open AreaOfEffect
    open Penetration
    open ResourceName

    type Weapon = {
        name: string
        oneHandedWeaponDice: DicePoolMod option
        twoHandedWeaponDice: DicePoolMod option
        penetration: Penetration
        range: Range
        damageTypes: DamageType Set
        engageableOpponents: EngageableOpponents
        dualWieldableBonus: DicePoolMod option
        areaOfEffectOption: AreaOfEffect option
        resourceNameOption: ResourceName option
    }

    let weaponToName weapon = weapon.name

module Container =
    type Container = {
        name: string
        weightCapacity: float
        volumeFtCubed: float
    }

    let containerToName container = container.name

module ItemTier =
    open DicePool

    type ItemTier = {
        name: string
        level: int
        baseDice: DicePool
        durabilityMax: uint
    }

module WeaponSpell =

    open DicePoolMod
    open Penetration
    open Range
    open EngageableOpponents
    open AreaOfEffect

    type WeaponSpell = {
        name: string
        oneHandedWeaponDice: DicePoolMod option
        twoHandedWeaponDice: DicePoolMod option
        dualWieldableBonus: DicePoolMod option
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

    let attributesToAttributeNames attributes = Seq.map (_.attributeName) attributes

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

module SkillName =
    type SkillName = string

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

    let attributeDeterminedDiceModToName attributeDeterminedDiceMod = attributeDeterminedDiceMod.name

    let attributeDeterminedDiceModEffectToEffectString attributeDeterminedDiceModEffect =
        let attributesString =
            String.concat "," attributeDeterminedDiceModEffect.attributesToEffect

        let dicePoolModString =
            dicePoolModToString attributeDeterminedDiceModEffect.dicePoolMod

        $"{dicePoolModString} {attributesString} ({attributeDeterminedDiceModEffect.name})"

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

    let physicalDefenseToName physicalDefense = physicalDefense.name

    let physicalDefenseEffectToEffectString defenseClass =
        $"{defenseClass.physicalDefense} Physical Defense"

module SkillDiceMod =
    open DurationAndSource
    open DicePoolMod

    type SkillDiceMod = {
        name: string
        skillToEffect: string
        diceMod: DicePoolMod
        durationAndSource: DurationAndSource
    }

    let skillDiceModToName skillDiceMod = skillDiceMod.name

    let skillDiceModEffectToEffectString skillDiceModEffect =
        $"{dicePoolModToString skillDiceModEffect.diceMod} {skillDiceModEffect.skillToEffect}"

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

// module MovementSpeedEffect =

//     open AttributeName
//     open Attribute
//     open Neg1To5
//     open Neg2To5
//     open CoreSkill
//     open SkillDiceMod
//     open AttributeDeterminedDiceMod
//     open DicePoolMod

//     type MovementSpeedCalculation = {
//         name: string
//         baseMovementSpeed: uint
//         governingAttribute: AttributeName
//         feetPerAttributeLvl: uint
//         governingSkill: string
//         feetPerSkillLvl: uint
//     }

//     let movementSpeedCalculationToName movementSpeedCalculation = movementSpeedCalculation.name

//     type CoreSkillAndAttributeData = {
//         coreSkillList: CoreSkill list
//         attributeList: Attribute list
//         skillDiceModEffectList: SkillDiceMod list
//         attributeDeterminedDiceModEffectList: AttributeDeterminedDiceMod list
//     }

//     let calculateMovementSpeed (coreSkillAndAttributeData: CoreSkillAndAttributeData) movementSpeedCalculation =

//         let attributeMod =
//             coreSkillAndAttributeData.attributeList
//             |> List.tryFind (fun attribute -> attribute.attributeName = movementSpeedCalculation.governingAttribute)
//             |> (fun attributeLevelOption ->
//                 match attributeLevelOption with
//                 | Some attribute -> neg2To5ToInt attribute.level * int movementSpeedCalculation.feetPerAttributeLvl
//                 | None -> 0)


//         let coreSkillMod =
//             coreSkillAndAttributeData.coreSkillList
//             |> List.tryFind (fun coreSkill -> coreSkill.skill.name = movementSpeedCalculation.governingSkill)
//             |> (fun coreSkillOption ->
//                 match coreSkillOption with
//                 | Some coreSkill ->
//                     neg1To5ToInt coreSkill.skill.level
//                     * int movementSpeedCalculation.feetPerSkillLvl
//                 | None -> 0)

//         let skillDiceModInt =
//             coreSkillAndAttributeData.skillDiceModEffectList
//             |> skillNameToSkillDiceMods movementSpeedCalculation.governingSkill
//             |> List.map dicePoolModToInt
//             |> List.sum

//         let attributeDeterminedDiceModInt =
//             coreSkillAndAttributeData.attributeDeterminedDiceModEffectList
//             |> attributeDeterminedDiceModsToDicePoolMods (Set.empty.Add(movementSpeedCalculation.governingAttribute))
//             |> List.map dicePoolModToInt
//             |> List.sum

//         let movementSpeed =
//             List.sum [
//                 int movementSpeedCalculation.baseMovementSpeed
//                 attributeMod
//                 coreSkillMod
//                 skillDiceModInt
//                 attributeDeterminedDiceModInt
//             ]

//         if movementSpeed >= 0 then movementSpeed else 0

//     let movementSpeedCalculationToSourceForDisplay movementSpeedCalculation =
//         $"{movementSpeedCalculation.baseMovementSpeed} ft (base), +{movementSpeedCalculation.feetPerAttributeLvl} ft (per {movementSpeedCalculation.governingAttribute}), +{movementSpeedCalculation.feetPerSkillLvl} ft (per {movementSpeedCalculation.governingSkill})"

module BaseDiceMod =
    open DurationAndSource
    open DicePool

    type BaseDiceMod = {
        name: string
        effectedSkillName: string
        baseDice: DicePool
        durationAndSource: DurationAndSource
    }

    let baseDiceModToName baseDiceMod = baseDiceMod.name

    let findBaseDiceForSkill skillName baseDiceModList =
        baseDiceModList
        |> List.tryFind (fun baseDiceEffect -> baseDiceEffect.effectedSkillName = skillName)
        |> (fun baseDiceEffectOption ->
            match baseDiceEffectOption with
            | Some baseDiceEffect -> baseDiceEffect.baseDice
            | None -> base3d6DicePool)

module TextEffect =
    open DurationAndSource

    type TextEffect = {
        name: string
        effect: string
        durationAndSource: DurationAndSource
    }

module Effect =
    open StringUtils
    open SkillDiceMod
    open AttributeStatAdjustment
    open PhysicalDefense
    open AttributeDeterminedDiceMod
    open Weapon
    open WeaponResource
    open Container
    open BaseDiceMod
    open TextEffect

    type Effect =
        | Weapon of Weapon
        | WeaponResource of WeaponResource
        | Container of Container
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
        | Container container -> container.name
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
        | SkillDiceMod skillAdjustment -> [ skillAdjustment ]
        | _ -> []

    let effectsToSkillDiceModEffectList = List.collect effectToSkillDiceModEffectList

    let effectToAttributeDeterminedDiceModEffectList (effect: Effect) =
        match effect with
        | AttributeDeterminedDiceMod addme -> [ addme ]
        | _ -> []

    let effectsToAttributeDeterminedDiceModEffectList =
        List.collect effectToAttributeDeterminedDiceModEffectList

    let effectToBaseDiceMod effect =
        match effect with
        | BaseDiceMod bdm -> [ bdm ]
        | _ -> []

    let effectsToBaseDiceModList = List.collect effectToBaseDiceMod

    open DicePoolMod

    let skillDiceModToTextEffect (sdm: SkillDiceMod) =
        (sdm.name, sprintf "%s to %s" (dicePoolModToString sdm.diceMod) sdm.skillToEffect, sdm.durationAndSource)

    let attributeDeterminedDiceModToNameAndEffect (addm: AttributeDeterminedDiceMod) =
        (addm.name,
         sprintf
             "%s to %s"
             (dicePoolModToString addm.dicePoolMod)
             (stringSeqToStringSeperatedByCommaAndSpace addm.attributesToEffect),
         addm.durationAndSource)

    let physicalDefenseToNameAndEffect (pd: PhysicalDefense) =
        (pd.name, sprintf "+%.2f to Physical Defense" pd.physicalDefense, pd.durationAndSource)

    let effectToTextEffect effect =
        match effect with
        | TextEffect te -> (te.name, te.effect, te.durationAndSource)
        | SkillDiceMod sdm -> skillDiceModToTextEffect sdm
        | AttributeDeterminedDiceMod addm -> attributeDeterminedDiceModToNameAndEffect addm
        | PhysicalDefense pd -> physicalDefenseToNameAndEffect pd
        |> (fun (nameString, effectString, durationAndSource) -> {
            name = nameString
            effect = effectString
            durationAndSource = durationAndSource
        })

// Item

module Item =
    open ItemTier
    open Weapon
    open WeaponResource
    open StringUtils
    open Effect

    type Item = {
        name: string
        itemEffectSet: Effect Set
        itemTier: ItemTier
        value: string
        weight: float
    }

    let sumItemListWeight itemList =
        if List.isEmpty itemList then
            0.0
        else
            List.sumBy (fun item -> item.weight) itemList

// let effectSetToString effectSet =
//     effectSet
//     |> Set.map effectToEffectName
//     |> stringSeqToStringSeperatedByCommaAndSpace

// let itemToWeaponEffects item : Weapon Set =
//     item.itemEffectSet
//     |> Set.fold
//         (fun acc itemEffect ->
//             match itemEffect with
//             | Weapon weapon -> acc.Add(weapon)
//             | _ -> acc)
//         Set.empty

// let itemToConduitSet item : Conduit Set =
//     item.itemEffectSet
//     |> Set.fold
//         (fun acc effect ->
//             match effect with
//             | Conduit conduit -> acc.Add(conduit)
//             | _ -> acc)
//         Set.empty

// let itemToWeaponResourceClasses item : WeaponResource Set =
//     item.itemEffectSet
//     |> Set.fold
//         (fun acc effect ->
//             match effect with
//             | WeaponResource weaponResource -> acc.Add(weaponResource)
//             | _ -> acc)
//         Set.empty

// let itemToItemNameAndContainerClasses item =
//     item.itemEffectSet
//     |> List.collect (fun itemClass ->
//         match itemClass with
//         | Container specifiedItemClass -> [ (item.name, specifiedItemClass) ]
//         | _ -> [])

// let itemToContainerClassNames item =
//     item.itemEffectSet
//     |> List.collect (fun itemClass ->
//         match itemClass with
//         | Container _ -> [ item.name ]
//         | _ -> [])

// let itemToItemEffectSubTypes (itemEffectToItemEffectSubType) (item: Item) =
//     item.itemEffectSet
//     |> List.collect (fun itemClass ->
//         match itemClass with
//         | ItemEffect itemEffect -> itemEffectToItemEffectSubType itemEffect
//         | _ -> [])

// let itemToItemNameAndItemEffectList (item: Item) =
//     item.itemEffectSet
//     |> List.collect (fun itemClass ->
//         match itemClass with
//         | ItemEffect itemEffect -> [ (item.name, itemEffect) ]
//         | _ -> [])

// let itemToSkillDiceModEffects =
//     itemToItemEffectSubTypes effectToSkillDiceModEffectList

// let itemToAttributeDeterminedDiceModEffects =
//     itemToItemEffectSubTypes effectToAttributeDeterminedDiceModEffectList

module ItemStack =
    open Item

    type ItemStack = { item: Item; quantity: uint }

    let itemStackListToItemList itemStackList =
        List.map (fun itemStack -> itemStack.item) itemStackList

    let sumItemStackListWeight (itemStackList: ItemStack list) =
        itemStackList
        |> List.map (fun itemStack -> itemStack.item.weight * (float itemStack.quantity))
        |> List.sum

// module Container =

//     open Container
//     open ItemStack

//     type Container = {
//         name: string
//         containerClass: ContainerClass
//         isEquipped: bool
//         itemStackList: ItemStack list
//     }

//     let itemNameAndContainerClassToContainer (itemName, containerClass: ContainerClass) = {
//         name = itemName
//         containerClass = containerClass
//         isEquipped = false
//         itemStackList = []
//     }

//     let sumContainerListWeight (containerList: Container list) =
//         containerList
//         |> List.map (fun container -> sumItemStackListWeight container.itemStackList)
//         |> List.sum


// ItemStat

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
            |> findBaseDiceForSkill skillName

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

    let calculateDicePool skillName levelDiceMod governingAttributeNameSet dicePoolCalculationData =
        createDicePoolModList skillName levelDiceMod governingAttributeNameSet dicePoolCalculationData
        |> dicePoolModListToDicePool

    let createSkillDicePool
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
        let skillLevelDiceMod = skillLevel |> neg1To5ToInt |> intToD6DicePoolMod
        calculateDicePool skillName skillLevelDiceMod governingAttributeNameSet dicePoolCalculationData

    let calculateVocationStatDicePool vocationStatName vocationStatLevel attributeSet dicePoolCalculationData =
        let vocationStatDiceMod = vocationStatLevel |> zeroToFiveToInt |> intToD6DicePoolMod
        calculateDicePool vocationStatName vocationStatDiceMod attributeSet dicePoolCalculationData

module Skill =
    open Neg1To5
    open DicePool
    open SkillName
    open DicePoolCalculation
    open AttributeName

    type Skill = {
        name: SkillName
        level: Neg1To5
        governingAttributeNames: AttributeName Set
        dicePool: DicePool
    //effectDicePoolModList: DicePoolMod List
    }

    let init name governingAttributes dicePoolCalculationData =
        let level = Neg1To5.init ()

        {
            name = name
            level = level
            governingAttributeNames = governingAttributes
            dicePool = calculateSkillDicePool name level governingAttributes dicePoolCalculationData
        }

module WeaponSkillData =
    open AttributeName

    type WeaponSkillData = {
        name: string
        governingAttributes: AttributeName Set
        governedWeapons: string Set
    }

// module WeaponSkill =
//     open VocationalSkill

//     type WeaponSkill = {
//         vocationalSkill: VocationalSkill
//         governedWeapons: string Set
//     }

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

    let determineGoverningCoreSkillLevelFloatForCalc governingCoreSkillLevel =
        match governingCoreSkillLevel with
        | NegOne -> 0.0
        | Neg1To5.Zero -> 0.5
        | positiveLevel -> neg1To5ToFloat positiveLevel

    let calcGoverningSkillMagicResource (governingCoreSkillLevel: Neg1To5) (governingCoreSkillDicePoolSize: uint) =
        governingCoreSkillDicePoolSize
        |> float
        |> (*) (determineGoverningCoreSkillLevelFloatForCalc governingCoreSkillLevel)
        |> (/) 2.0

    let calculateMagicResourcePool
        vocationLevel
        vocationDicePoolSize
        governingCoreSkillLevel
        governingCoreSkillDicePoolSize
        =
        calculateVocationMagicResource vocationLevel vocationDicePoolSize
        |> (+) (calcGoverningSkillMagicResource governingCoreSkillLevel governingCoreSkillDicePoolSize)
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
        magicSkillDataSet: MagicSkillData Set
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

module MagicVocation =
    open VocationStat
    open MagicSystem
    open MagicVocationSkill

    type MagicVocation = {
        vocationStat: VocationStat
        magicVocationSkills: MagicVocationSkill Set
        magicSystem: MagicSystem
        magicResourceCap: uint
        currentMagicResource: uint
    }

module Vocation =
    open MundaneVocation
    open MagicVocation

    type Vocation =
        | MundaneVocation of MundaneVocation
        | MagicVocation of MagicVocation

module Vocations =
    open MundaneVocation
    open MagicVocation

    type Vocations = {
        mundaneVocations: MundaneVocation List
        magicVocations: MagicVocation List
    }

module WeightClass =
    open AttributeDeterminedDiceMod

    type WeightClass = {
        name: string
        bottomPercent: float
        topPercent: float
        attributeDeterminedDiceModEffect: AttributeDeterminedDiceMod
    }

    let determineWeightClass (maxCarryWeight: float) (inventoryWeight: float) (weightClassList: WeightClass list) =

        let percentOfMaxCarryWeight = inventoryWeight / maxCarryWeight

        List.find
            (fun weightClass ->
                (weightClass.topPercent >= percentOfMaxCarryWeight)
                && (percentOfMaxCarryWeight >= weightClass.bottomPercent))
            weightClassList

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

    type CombatRoll = {
        name: string
        dicePool: DicePool
        weaponAndResourceDicePoolModString: string
        calculatedRange: CalculatedRange
        penetration: Penetration
        damageTypeSet: DamageType Set
        setAreaOfEffectOption: SetAreaOfEffect Option
        calculatedEngageableOpponents: CalculatedEngageableOpponents
        eoName: string option
    }

    open ItemStack

    open WeaponResource
    open AreaOfEffect

    let createWeaponCombatRoll
        (weaponName: string)
        (weaponPenetration: Penetration)
        (weaponRange: Range)
        (weaponDamageTypeSet: DamageType Set)
        (weaponEO: EngageableOpponents)
        (weaponAOEOption: AreaOfEffect option)
        (baseDice: DicePool)
        (skillDicePoolModList: DicePoolMod List)
        (resource: WeaponResource option)
        (weaponHandedSuffixString: string)
        (weaponDiceMod: DicePoolMod)
        (offHandWeaponDiceMod: DicePoolMod)
        : CombatRoll =

        let (resourceDesc, resourceDice, resourcePenetration, resourceRange, resourceDamageTypeSet, resourceAreaOfEffect) =
            weaponResourceClassOptionToWeaponResourceClass resource

        let dicePool =
            modifyDicePoolByDicePoolModList baseDice (skillDicePoolModList @ [ weaponDiceMod; offHandWeaponDiceMod ])

        let numDice = dicePool |> dicePoolToNumDice

        {
            name = weaponName + resourceDesc + weaponHandedSuffixString
            dicePool = dicePool
            weaponAndResourceDicePoolModString = dicePoolModListToString [ weaponDiceMod; resourceDice ]
            calculatedRange = determineGreatestRange numDice weaponRange resourceRange
            penetration = weaponPenetration + resourcePenetration
            damageTypeSet = Set.union weaponDamageTypeSet resourceDamageTypeSet
            setAreaOfEffectOption = compareAndDetermineAOEShapeOption numDice weaponAOEOption resourceAreaOfEffect
            calculatedEngageableOpponents = determineEngageableOpponents numDice weaponEO
            eoName =
                (match weaponEO with
                 | Calculation eoCalc -> Some eoCalc.name
                 | Calculated _ -> None)
        }

    open Weapon
    open ItemTier

    let temp
        preloadedCreateWeaponCombatRoll
        (handedVariationString: string)
        (weaponHandedDicePoolModOption: DicePoolMod option)
        =
        match weaponHandedDicePoolModOption with
        | Some weaponHandedDice -> [
            preloadedCreateWeaponCombatRoll handedVariationString weaponHandedDice emptyDicePoolMod
          ]
        | None -> []

    let temp2
        preloadedCreateWeaponCombatRoll
        (handedVariationString: string)
        (weaponHandedDicePoolModOption: DicePoolMod option)
        (offHandedDicePoolModOption: DicePoolMod option)
        =
        match weaponHandedDicePoolModOption, offHandedDicePoolModOption with
        | Some weaponHandedDice, Some offHandedDicePoolMod -> [
            preloadedCreateWeaponCombatRoll handedVariationString weaponHandedDice offHandedDicePoolMod
          ]
        | _, _ -> []

    let createCombatRoll
        (weapon: Weapon)
        (baseDice: DicePool)
        (skillDicePoolModList: DicePoolMod List)
        (weaponResource: WeaponResource option)
        : CombatRoll list =

        let preloadedCreateWeaponCombatRoll =
            createWeaponCombatRoll
                weapon.name
                weapon.penetration
                weapon.range
                weapon.damageTypes
                weapon.engageableOpponents
                weapon.areaOfEffectOption
                baseDice
                skillDicePoolModList
                weaponResource

        [
            (temp preloadedCreateWeaponCombatRoll " (one-handed)" weapon.oneHandedWeaponDice)
            (temp preloadedCreateWeaponCombatRoll " (two-handed)" weapon.twoHandedWeaponDice)
            (temp2
                preloadedCreateWeaponCombatRoll
                " (dual-wielded)"
                weapon.oneHandedWeaponDice
                weapon.dualWieldableBonus)
        ]
        |> List.collect id

    open WeaponSkillData

    let createWeaponItemCombatRolls
        (equipmentList: ItemStack List)
        (weaponSkillList: Skill List)
        (weaponSkillDataMap: Map<string, WeaponSkillData>)
        (dicePoolCalculationData: DicePoolCalculationData)
        : CombatRoll List =

        let weaponResourceList =
            equipmentList
            |> List.collect (fun itemStack ->
                itemStack.item.itemEffectSet
                |> Set.toList
                |> List.collect (fun effect ->
                    match effect with
                    | WeaponResource weaponResource -> [ weaponResource ]
                    | _ -> []))

        let tryFindWeaponSkill weaponSkillName (skills: Skill List) =
            skills |> List.tryFind (fun skill -> skill.name = weaponSkillName)

        equipmentList
        |> List.collect (fun itemStack ->

            itemStack.item.itemEffectSet
            |> Set.toList
            |> List.collect (fun effect ->
                match effect with
                | Weapon weapon -> (itemStack.item.name, weapon, itemStack.item.itemTier) |> List.singleton
                | _ -> List.empty))

        |> List.collect (fun (itemName, weapon, itemTier) ->

            match weapon.resourceNameOption with
            | Some resourceClass ->
                weaponResourceList
                |> List.collect (fun weaponResource ->
                    if weaponResource.resourceName = resourceClass then
                        (itemName, weapon, Some weaponResource, itemTier) |> List.singleton
                    else
                        List.empty)
            | None -> List.singleton (itemName, weapon, None, itemTier))

        |> List.collect (fun (itemName, weapon, weaponResourceOption, itemTier) ->


            weaponSkillDataMap.TryFind weapon.name
            |> function
                | None -> Skill.init weapon.name Set.empty dicePoolCalculationData
                | Some weaponSkillData ->
                    tryFindWeaponSkill weaponSkillData.name weaponSkillList
                    |> function
                        | Some vocationalSkill -> vocationalSkill
                        | None ->
                            Skill.init weaponSkillData.name weaponSkillData.governingAttributes dicePoolCalculationData

            |> (fun skill ->
                createSkillDicePool skill.name skill.level skill.governingAttributeNames dicePoolCalculationData)
            |> (fun skillDicePoolModList ->
                createCombatRoll weapon itemTier.baseDice skillDicePoolModList weaponResourceOption))

module CharacterInformation =
    type CharacterInformation = {
        notes: string
        appearance: string
        disposition: string
        beliefsAndMorality: string
        goalsAndAspirations: string
        backstory: string
    }

module Character =
    open Attribute
    open Skill
    open Vocation
    open DicePoolCalculation
    open ItemStack
    open CombatRoll
    open CharacterInformation
    open Effect
    open CombatSpeed
    open MagicVocationSkill
    open MundaneVocationSkill

    type Character = {
        name: string
        attributes: Attribute Set
        coreSkills: Skill Set
        vocationList: Vocation list
        equipmentList: ItemStack list
        combatRollList: CombatRoll List
        characterInformation: CharacterInformation
        characterEffects: Effect List
        combatSpeeds: CombatSpeed List
    }

    let characterToDicePoolCalculationData character = {
        effects = character.characterEffects
        attributes = character.attributes
    }

    let vocationListToWeaponSkillList (vocationList: Vocation List) =
        vocationList
        |> List.collect (fun vocation ->
            match vocation with
            | MagicVocation magicVocation ->
                magicVocationSkillsToSkills magicVocation.magicVocationSkills |> List.ofSeq
            | MundaneVocation mundaneVocation ->
                mundaneVocationSkillsToSkills mundaneVocation.mundaneVocationSkills
                |> List.ofSeq)