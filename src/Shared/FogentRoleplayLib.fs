namespace FogentRoleplayLib
// Utils
module StringUtils =
    open System.Text.RegularExpressions

    let isNumeric (number: string) =
        let regex = Regex(@"^[0-9]+$")
        regex.IsMatch(number)

    let stringListToStringSeperatedByCommas (damageTypes: string list) = String.concat ", " damageTypes


module MathUtils =
    open System

    let multiply x y = x * y

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

module TypeUtils =

    let stringListToTypeMap (stringTypeArray: string list) =
        List.zip stringTypeArray stringTypeArray |> Map.ofList

// Base Building Blocks

module Penetration =

    type Penetration = uint

module DamageType =

    type DamageType = string

    let stringAndMapToDamageTypeList (damageTypeMap: Map<string, DamageType>) (damageTypesString: string) =
        if damageTypesString.Length = 0 then
            []
        else
            damageTypesString.Split ", "
            |> List.ofArray
            |> List.map (fun (damageTypeString) -> damageTypeMap.Item damageTypeString)

module EngageableOpponents =

    open MathUtils
    open StringUtils

    type EngageableOpponentsCalculation = {
        name: string
        combatRollDivisor: uint
        maxEO: uint option
    }

    let eoCalculationListToMap eoCalculationList =
        eoCalculationList
        |> List.map (fun (eoCalculation) -> eoCalculation.name, eoCalculation)
        |> Map.ofList

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

module BattleMapUOM =
    let feetPerBattleMapUOM = 5u

module Range =

    open System
    open MathUtils

    type CalculatedRange = {
        name: string
        effectiveRange: uint
        maxRange: uint
    }

    type RangeCalculation = {
        name: string
        numDicePerEffectiveRangeUnit: uint
        ftPerEffectiveRangeUnit: uint
        roundEffectiveRangeUp: bool // If true, round up the effective range if decimal in calculation, otherwise round down
        maxRange: uint
    }

    type Range =
        | CalculatedRange of CalculatedRange
        | RangeCalculation of RangeCalculation

    //type RangeAdjustment = int

    let calculatedRangeToString calculatedRange =
        sprintf "%u/%u" calculatedRange.effectiveRange calculatedRange.maxRange

    let calculateRangeCalculation numDice rangeCalculation = {
        name = rangeCalculation.name
        effectiveRange =
            if rangeCalculation.roundEffectiveRangeUp then
                float numDice / float rangeCalculation.numDicePerEffectiveRangeUnit
                |> Math.Ceiling
                |> uint
                |> multiply rangeCalculation.ftPerEffectiveRangeUnit
            else
                float numDice / float rangeCalculation.numDicePerEffectiveRangeUnit
                |> Math.Floor
                |> uint
                |> multiply rangeCalculation.ftPerEffectiveRangeUnit
        maxRange = rangeCalculation.maxRange
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
        List.map
            (fun (rangeCalculation: RangeCalculation) -> rangeCalculation.name, RangeCalculation rangeCalculation)
            rangeCalculationList
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

module AreaOfEffect =
    type AreaOfEffect =
        | Cone
        | Sphere

    let AreaOfEffectOptionMap =
        Map [ ("Cone", Some Cone); ("Sphere", Some Sphere); ("None", None) ]

module CalculatedAOE =
    open System
    open AreaOfEffect
    open BattleMapUOM

    type CalculatedCone = {
        area: float
        distance: uint
        angle: float
    }

    type CalculatedSphere = { area: float; radius: float }

    type CalculatedAOE =
        | ConeToCalculatedCone of CalculatedCone
        | SphereToCalculatedSphere of CalculatedSphere

    let calculatedConeToString decimalPlaces (calculatedCone: CalculatedCone) =
        let decimalLimitedArea =
            calculatedCone.area.ToString("F" + decimalPlaces.ToString())

        let decimalLimitedAngle =
            calculatedCone.angle.ToString("F" + decimalPlaces.ToString())

        sprintf
            "area: %s ft^2, distance: %u ft, angle: %s θ"
            decimalLimitedArea
            calculatedCone.distance
            decimalLimitedAngle

    let calculatedSphereToString decimalPlaces calculatedSphere =
        let decimalLimitedArea =
            calculatedSphere.area.ToString("F" + decimalPlaces.ToString())

        let decimalLimitedRadius =
            calculatedSphere.radius.ToString("F" + decimalPlaces.ToString())

        sprintf "area: %s ft^2, radius: %s ft" decimalLimitedArea decimalLimitedRadius

    let calculatedAOEToString calculatedAOE =
        let decimalPlaces = 1

        match calculatedAOE with
        | ConeToCalculatedCone calculatedCone -> calculatedConeToString decimalPlaces calculatedCone
        | SphereToCalculatedSphere sphereShape -> calculatedSphereToString decimalPlaces sphereShape


    let calculatedAOEOptionToString shapeOption =
        match shapeOption with
        | Some shape -> calculatedAOEToString shape
        | None -> ""

    let calcConeArea (distance: uint) (angle: float) : float =
        float (distance * distance) * Math.Tan(angle / 2.0)

    let calcConeDistance (area: uint) (angle: float) =
        uint (Math.Sqrt(float area / Math.Tan(angle / 2.)))

    let calcConeAngle (area: uint) (distance: uint) =
        2. * Math.Atan(Math.Sqrt(float area / float (distance * distance)))

    let calcCone (numDice: uint) : CalculatedCone =
        let distance = numDice * feetPerBattleMapUOM
        let angle = 53.0

        {
            area = calcConeArea distance angle
            distance = distance
            angle = angle
        }

    let calcCircle (numDice: uint) : CalculatedSphere =
        let radius: float = 2.5 * float numDice

        {
            area = 2.0 * Math.PI * (radius ** 2)
            radius = radius
        }

    let calcShape (numDice: uint) (aoe: AreaOfEffect) : CalculatedAOE =
        match aoe with
        | Cone -> ConeToCalculatedCone(calcCone numDice)
        | Sphere -> SphereToCalculatedSphere(calcCircle numDice)

    let determineAOEShapeOption numDice aoe =
        match aoe with
        | Some aoe -> Some(calcShape numDice aoe)
        | None -> None

    let compareAndDetermineAOEShapeOption
        (numDice: uint)
        (aoe: AreaOfEffect option)
        (resourceAOE: AreaOfEffect option)
        : CalculatedAOE option =
        match resourceAOE with
        | Some resourceAOE -> Some(calcShape numDice resourceAOE)
        | None -> determineAOEShapeOption numDice aoe

// Item Building

module ResourceClass =
    type ResourceClass = string

// Character Building Blocks

module AttributeName =
    type AttributeName = string

module Attribute =
    open Neg2To5
    open AttributeName

    type Attribute = {
        attributeName: AttributeName
        level: Neg2To5
    }

module Skill =
    open Neg1To5
    open DicePool
    open DicePoolMod
    open Attribute

    type Skill = {
        name: string
        level: Neg1To5
        dicePool: DicePool
    }

    type DicePoolCalculationData = {
        baseDice: DicePool option
        AttributeList: Attribute list
        injuryDicePenalty: DicePoolPenalty
        weightClassDicePenalty: DicePoolPenalty
        itemEffectDicePoolMod: DicePoolMod
    }

    let findSkillLvlWithDefault skillName (defaultLvl: Neg1To5) (skillList: Skill list) =
        skillList
        |> List.filter (fun skill -> skill.name = skillName)
        |> (fun list ->
            if list.Length = 0 then
                defaultLvl
            else
                list
                |> List.maxBy (fun skill -> skill.level)
                |> (fun skillList -> skillList.level))

module CoreSkill =
    open AttributeName
    open Skill
    open DicePoolMod
    open DicePool
    open Neg1To5
    open Neg2To5

    type CoreSkill = {
        skill: Skill
        governingAttributeName: AttributeName
    }

    let calculateCoreSkillDicePool
        (dicePoolCalculationData: DicePoolCalculationData)
        (skillLevel: Neg1To5)
        (skillGoveringAttribute: AttributeName)
        =

        let attribute =
            dicePoolCalculationData.AttributeList
            |> List.find (fun attribute -> attribute.attributeName = skillGoveringAttribute)

        modifyDicePoolByDicePoolModList (dicePoolCalculationData.baseDice |> Option.defaultValue baseDicePool) [
            skillLevel |> neg1To5ToInt |> intToD6DicePoolMod
            attribute.level |> neg2To5ToInt |> intToD6DicePoolMod
            dicePoolCalculationData.injuryDicePenalty |> RemoveDice
            dicePoolCalculationData.itemEffectDicePoolMod
            dicePoolCalculationData.weightClassDicePenalty |> RemoveDice
        ]

module VocationalSkill =
    open Skill
    open AttributeName

    type VocationalSkill = {
        skill: Skill
        governingAttributes: AttributeName list
    }
// Magic

module MagicResource =
    type MagicResource = string

module MagicResourcePool =

    open MagicResource

    type MagicResourcePool = {
        magicResource: MagicResource
        remainingResources: uint
        poolMax: uint
    }

module MagicSkillData =
    open DamageType
    open MagicResource

    type MagicSkillData = {
        name: string
        damageTypes: DamageType list
        isMeleeCapable: bool
        isRangeCapable: bool
        magicResource: MagicResource
    }

module MagicSkill =
    open VocationalSkill
    open DamageType
    open MagicResource

    type MagicSkill = {
        vocationalSkill: VocationalSkill
        damageTypes: DamageType list
        isMeleeCapable: bool
        isRangeCapable: bool
        magicResource: MagicResource
    }

// Larger Character Building Blocks

module AttributeAndCoreSkills =
    open AttributeName
    open Neg2To5
    open Attribute
    open CoreSkill

    type AttributeAndCoreSkills = {
        attributeStat: Attribute
        coreSkills: CoreSkill list
    }

    let defaultAttributeAndCoreSkills
        (coreSkillList: CoreSkill list)
        (attribute: AttributeName)
        : AttributeAndCoreSkills =
        {
            attributeStat = {
                attributeName = attribute
                level = Zero
            }
            coreSkills =
                List.collect
                    (fun coreSkill ->
                        if coreSkill.governingAttributeName = attribute then
                            [ coreSkill ]
                        else
                            [])
                    coreSkillList
        }

module VocationSkill =
    open VocationalSkill
    open MagicSkill

    type VocationSkill =
        | VocationalSkill of VocationalSkill
        | WeaponSkill of VocationalSkill
        | MagicSkill of MagicSkill

module Vocation =
    open ZeroToFive
    open DicePool
    open VocationSkill

    type Vocation = {
        name: string
        level: ZeroToFive
        dicePool: DicePool
        vocationSkillList: VocationSkill list
    }

// ItemStat

module WeaponClass =
    open DicePoolMod
    open Range
    open DamageType
    open EngageableOpponents
    open AreaOfEffect
    open Penetration
    open ResourceClass

    type WeaponClass = {
        name: string
        oneHandedWeaponDice: DicePoolMod option
        twoHandedWeaponDice: DicePoolMod
        penetration: Penetration
        range: Range
        damageTypes: DamageType list
        engageableOpponents: EngageableOpponents
        dualWieldableBonus: DicePoolMod option
        areaOfEffect: AreaOfEffect option
        resourceClass: ResourceClass option
    }

module Character =
    open AttributeName
    open CoreSkill
    open AttributeAndCoreSkills
    open Vocation

    type Character = {
        name: string
        attributeAndCoreSkillsList: AttributeAndCoreSkills list
        vocationList: Vocation list
    }

    let defaultAttributeAndCoreSkillsList (attributeList: AttributeName list) (coreSkillList: CoreSkill list) =
        List.map (defaultAttributeAndCoreSkills coreSkillList) attributeList