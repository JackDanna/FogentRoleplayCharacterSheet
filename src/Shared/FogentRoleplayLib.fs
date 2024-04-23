namespace FogentRoleplayLib
// Utils
module StringUtils =
    open System.Text.RegularExpressions

    let isNumeric (number: string) =
        let regex = Regex(@"^[0-9]+$")
        regex.IsMatch(number)

    let stringSeqToStringSeperatedByCommas (stringList: string seq) = String.concat ", " stringList

    let stringSetToStringSeperatedByCommas stringSet =
        stringSet |> List.ofSeq |> stringSeqToStringSeperatedByCommas

    let mapAndStringToValueSet (map: Map<string, 'a>) (input: string) =
        if input.Length = 0 then
            Set.empty
        else
            String.filter ((<>) ' ') input
            |> (fun s -> s.Split(',', System.StringSplitOptions.RemoveEmptyEntries))
            |> Set.ofArray
            |> Set.map (fun attributeString -> map.Item attributeString)


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

    let ZeroToFiveToNeg1To5 zeroToFive =
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

    let emptyDicePoolMod = createD6DicePoolMod 0u

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
            createD6DicePoolMod (uint num)

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

module Conduit =

    type Conduit = {
        name: string
        magicSkillEffected: string
    }

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

    let weaponResourceClassOptionToWeaponResourceClass (resource: WeaponResource option) =
        match resource with
        | Some resource ->
            (" (" + resource.name + ")",
             resource.dicePoolMod,
             resource.penetration,
             resource.rangeOption,
             resource.damageTypeSet,
             resource.NamedAreaOfEffectOption)
        | None -> ("", createD6DicePoolMod (0u), 0u, None, Set.empty, None)

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
        governingSkillName: string
        resourceNameOption: ResourceName option
    }

module ContainerClass =
    type ContainerClass = {
        name: string
        weightCapacity: float
        volumeFtCubed: float
    }

module ItemTier =
    open DicePool

    type ItemTier = {
        name: string
        level: int
        baseDice: DicePool
        durabilityMax: uint
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

    let findAttributeWithAttributeName attributeSet attributeName =
        Set.filter (fun attribute -> attribute.attributeName = attributeName) attributeSet

    let collectAttributesWithAttributeNames attributeSet attributeNameSet =
        Set.fold
            (fun acc attributeName -> attributeName |> findAttributeWithAttributeName attributeSet |> Set.union acc)
            Set.empty
            attributeNameSet

    let attributesToAttributeNames attributes =
        Set.map (fun (attribute: Attribute) -> attribute.attributeName) attributes


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

// let governingAttributesToDicePoolMod (attributes: Attribute list) (governingAttributes: AttributeName list) =
//     attributes
//     |> List.filter (fun attribute -> (List.contains attribute.attributeName governingAttributes))
//     |> List.map (fun governingAttribute -> neg1To5ToD6DicePoolMod governingAttribute.level)

module Skill =
    open Neg1To5
    open DicePool
    open DicePoolMod

    type SkillName = string

    type Skill = {
        name: SkillName
        level: Neg1To5
        baseDice: DicePool
        dicePoolModList: DicePoolMod List
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

    type CoreSkill = {
        skill: Skill
        governingAttributeName: AttributeName
    }

module VocationalSkill =
    open Skill
    open AttributeName

    type VocationalSkill = {
        skill: Skill
        governingAttributeNames: AttributeName Set
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
        damageTypes: DamageType Set
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
        damageTypeSet: DamageType Set
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

    let vocationSkillToVocationalSkill vocationSkill =
        match vocationSkill with
        | VocationalSkill vocationalSkill -> vocationalSkill
        | WeaponSkill vocationalSkill -> vocationalSkill
        | MagicSkill magicSkill -> magicSkill.vocationalSkill

module Vocation =
    open ZeroToFive
    open DicePool
    open DicePoolMod
    open VocationSkill
    open AttributeName

    type Vocation = {
        name: string
        governingAttributeNameSet: AttributeName Set
        level: ZeroToFive
        baseDice: DicePool
        dicePoolModList: DicePoolMod List
        vocationSkillList: VocationSkill list
    }

    let vocationListToVocationSkillList vocationList =
        vocationList |> List.collect (_.vocationSkillList)


// Effects

module AttributeDeterminedDiceModEffect =
    open AttributeName
    open DicePoolMod

    type AttributeDeterminedDiceMod = {
        name: string
        attributesToEffect: AttributeName Set
        dicePoolMod: DicePoolMod
    }

    let attributeDeterminedDiceModEffectToEffectString attributeDeterminedDiceModEffect =
        let attributesString =
            String.concat "," attributeDeterminedDiceModEffect.attributesToEffect

        let dicePoolModString =
            dicePoolModToString attributeDeterminedDiceModEffect.dicePoolMod

        $"{dicePoolModString} {attributesString} ({attributeDeterminedDiceModEffect.name})"

    let collectAttributeDeterminedDiceMod
        (governingAttributesOfSkill: AttributeName Set)
        attributeDeterminedDiceModList
        =
        attributeDeterminedDiceModList
        |> List.filter (fun attributeDeterminedDiceMod ->
            attributeDeterminedDiceMod.attributesToEffect
            |> Set.exists (fun attributeName -> Set.contains attributeName governingAttributesOfSkill))
        |> List.map (fun attributeDeterminedDiceMod -> attributeDeterminedDiceMod.dicePoolMod)

module PhysicalDefenseEffect =

    type PhysicalDefense = { name: string; physicalDefense: float }

    let physicalDefenseEffectToEffectString defenseClass =
        $"{defenseClass.physicalDefense} Physical Defense"

module SkillDiceModEffect =
    open DicePoolMod

    type SkillDiceMod = {
        name: string
        skillToEffect: string
        diceMod: DicePoolMod
    }

    let skillDiceModEffectToEffectString skillDiceModEffect =
        $"{dicePoolModToString skillDiceModEffect.diceMod} {skillDiceModEffect.skillToEffect}"

    let collectSkillAdjustmentDiceMods skillName skillAdjustmentList =
        skillAdjustmentList
        |> List.filter (fun skillAdjustment -> skillAdjustment.skillToEffect = skillName)
        |> List.map (fun skillAdjustment -> skillAdjustment.diceMod)

module AttributeStatAdjustmentEffect =

    open AttributeName

    type AttributeStatAdjustment = {
        name: string
        attribute: AttributeName
        adjustment: int
    }

    let attributeStatAdjustmentToEffectString attributeStatAdjustment =
        $"{attributeStatAdjustment.adjustment} {attributeStatAdjustment.attribute}"

module MovementSpeedEffect =

    open AttributeName
    open Attribute
    open Neg1To5
    open Neg2To5
    open CoreSkill
    open SkillDiceModEffect
    open AttributeDeterminedDiceModEffect
    open DicePoolMod

    type MovementSpeedCalculation = {
        name: string
        baseMovementSpeed: uint
        governingAttribute: AttributeName
        feetPerAttributeLvl: uint
        governingSkill: string
        feetPerSkillLvl: uint
    }

    type CoreSkillAndAttributeData = {
        coreSkillList: CoreSkill list
        attributeList: Attribute list
        skillDiceModEffectList: SkillDiceMod list
        attributeDeterminedDiceModEffectList: AttributeDeterminedDiceMod list
    }

    let calculateMovementSpeed (coreSkillAndAttributeData: CoreSkillAndAttributeData) movementSpeedCalculation =

        let attributeMod =
            coreSkillAndAttributeData.attributeList
            |> List.tryFind (fun attribute -> attribute.attributeName = movementSpeedCalculation.governingAttribute)
            |> (fun attributeLevelOption ->
                match attributeLevelOption with
                | Some attribute -> neg2To5ToInt attribute.level * int movementSpeedCalculation.feetPerAttributeLvl
                | None -> 0)


        let coreSkillMod =
            coreSkillAndAttributeData.coreSkillList
            |> List.tryFind (fun coreSkill -> coreSkill.skill.name = movementSpeedCalculation.governingSkill)
            |> (fun coreSkillOption ->
                match coreSkillOption with
                | Some coreSkill ->
                    neg1To5ToInt coreSkill.skill.level
                    * int movementSpeedCalculation.feetPerSkillLvl
                | None -> 0)

        let skillDiceModInt =
            coreSkillAndAttributeData.skillDiceModEffectList
            |> collectSkillAdjustmentDiceMods movementSpeedCalculation.governingSkill
            |> List.map dicePoolModToInt
            |> List.sum

        let attributeDeterminedDiceModInt =
            coreSkillAndAttributeData.attributeDeterminedDiceModEffectList
            |> collectAttributeDeterminedDiceMod (Set.empty.Add(movementSpeedCalculation.governingAttribute))
            |> List.map dicePoolModToInt
            |> List.sum

        let movementSpeed =
            List.sum [
                int movementSpeedCalculation.baseMovementSpeed
                attributeMod
                coreSkillMod
                skillDiceModInt
                attributeDeterminedDiceModInt
            ]

        if movementSpeed >= 0 then movementSpeed else 0

    let movementSpeedCalculationToSourceForDisplay movementSpeedCalculation =
        $"{movementSpeedCalculation.baseMovementSpeed} ft (base), +{movementSpeedCalculation.feetPerAttributeLvl} ft (per {movementSpeedCalculation.governingAttribute}), +{movementSpeedCalculation.feetPerSkillLvl} ft (per {movementSpeedCalculation.governingSkill})"

module Effect =
    open SkillDiceModEffect
    open AttributeStatAdjustmentEffect
    open PhysicalDefenseEffect
    open AttributeDeterminedDiceModEffect
    open MovementSpeedEffect
    open Weapon
    open WeaponResource
    open Conduit
    open ContainerClass

    type Effect =
        | Weapon of Weapon
        | Conduit of Conduit
        | WeaponResource of WeaponResource
        | Container of ContainerClass
        | SkillDiceMod of SkillDiceMod
        | AttributeStatAdjustment of AttributeStatAdjustment
        | PhysicalDefense of PhysicalDefense
        | AttributeDeterminedDiceMod of AttributeDeterminedDiceMod
        | MovementSpeedCalculation of MovementSpeedCalculation

    let effectToEffectName effect =
        match effect with
        | Weapon weapon -> weapon.name
        | Conduit conduit -> conduit.name
        | WeaponResource weaponResource -> weaponResource.name
        | Container container -> container.name
        | SkillDiceMod skillDiceModEffect -> skillDiceModEffect.name
        | AttributeStatAdjustment attributeStatAdjustment -> attributeStatAdjustment.name
        | PhysicalDefense defenseClass -> defenseClass.name
        | AttributeDeterminedDiceMod addme -> addme.name
        | MovementSpeedCalculation msc -> msc.name

    let effectToSkillDiceModEffectList (effect: Effect) =
        match effect with
        | SkillDiceMod skillAdjustment -> [ skillAdjustment ]
        | _ -> []

    let effectToAttributeDeterminedDiceModEffectList (effect: Effect) =
        match effect with
        | AttributeDeterminedDiceMod addme -> [ addme ]
        | _ -> []

// Item

module Item =
    open ItemTier
    open Weapon
    open WeaponResource
    open Conduit
    open ContainerClass
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

    let effectSetToString effectSet =
        effectSet |> Set.map effectToEffectName |> String.concat ", "

    let itemToWeaponEffects item : Weapon Set =
        item.itemEffectSet
        |> Set.fold
            (fun acc itemEffect ->
                match itemEffect with
                | Weapon weapon -> acc.Add(weapon)
                | _ -> acc)
            Set.empty

    let itemToConduitSet item : Conduit Set =
        item.itemEffectSet
        |> Set.fold
            (fun acc effect ->
                match effect with
                | Conduit conduit -> acc.Add(conduit)
                | _ -> acc)
            Set.empty

    let itemToWeaponResourceClasses item : WeaponResource Set =
        item.itemEffectSet
        |> Set.fold
            (fun acc effect ->
                match effect with
                | WeaponResource weaponResource -> acc.Add(weaponResource)
                | _ -> acc)
            Set.empty

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
    open Neg1To5
    open AttributeName
    open ZeroToFive

    type DicePoolCalculationData = {
        attributeSet: Attribute Set
        injuryDicePenalty: DicePoolPenalty
        weightClassDicePenalty: DicePoolPenalty
        itemEffectDicePoolMod: DicePoolMod
    }

    let createDicePoolModList
        (dicePoolCalculationData: DicePoolCalculationData)
        (skillLevel: int)
        (goveringAttributes: AttributeName Set)
        =

        [
            skillLevel |> intToD6DicePoolMod
            (goveringAttributes
             |> sumGoverningAttributeD6DiceMods dicePoolCalculationData.attributeSet)
            dicePoolCalculationData.injuryDicePenalty |> RemoveDice
            dicePoolCalculationData.itemEffectDicePoolMod
            dicePoolCalculationData.weightClassDicePenalty |> RemoveDice
        ]

    let createCoreSkillDicePoolModList
        (dicePoolCalculationData: DicePoolCalculationData)
        (skillLevel: Neg1To5)
        (skillGoveringAttributeName: AttributeName)
        =

        createDicePoolModList
            dicePoolCalculationData
            (skillLevel |> neg1To5ToInt)
            (Set.empty.Add(skillGoveringAttributeName))

    let calculateVocationalSkillDicePoolModList
        (dicePoolCalculationData: DicePoolCalculationData)
        (skillLevel: Neg1To5)
        (skillGoveringAttributeNames: AttributeName Set)
        =

        createDicePoolModList dicePoolCalculationData (skillLevel |> neg1To5ToInt) skillGoveringAttributeNames

    let calculateVocationDicePoolModList
        (dicePoolCalculationData: DicePoolCalculationData)
        (skillLevel: ZeroToFive)
        (skillGoveringAttributeNames: AttributeName Set)
        =

        createDicePoolModList
            dicePoolCalculationData
            (skillLevel |> zeroToFiveToUint |> int)
            skillGoveringAttributeNames

module WeightClass =
    open AttributeDeterminedDiceModEffect

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

    type CombatRoll = {
        name: string
        dicePool: DicePool
        weaponAndResourceDicePoolMod: string
        calculatedRange: CalculatedRange
        penetration: Penetration
        damageTypeSet: DamageType Set
        setAreaOfEffectOption: SetAreaOfEffect Option
        calculatedEngageableOpponents: CalculatedEngageableOpponents
    }

    open VocationSkill
    open ItemStack

    type CombatRollData = {
        vocationSkillList: VocationSkill list
        equipmentList: ItemStack List
    }

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
            weaponAndResourceDicePoolMod = dicePoolModListToString [ weaponDiceMod; resourceDice ]
            calculatedRange = determineGreatestRange numDice weaponRange resourceRange
            penetration = weaponPenetration + resourcePenetration
            damageTypeSet = Set.union weaponDamageTypeSet resourceDamageTypeSet
            setAreaOfEffectOption = compareAndDetermineAOEShapeOption numDice weaponAOEOption resourceAreaOfEffect
            calculatedEngageableOpponents = determineEngageableOpponents numDice weaponEO
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

    open Effect

    let createWeaponItemCombatRolls
        (equipmentList: ItemStack List)
        (vocationSkillList: VocationSkill List)
        : CombatRoll list =

        let weaponSkillList =
            vocationSkillList
            |> List.collect (fun vocationSkill ->
                match vocationSkill with
                | WeaponSkill weaponSkill -> [ weaponSkill ]
                | _ -> [])

        let weaponResourceList =
            equipmentList
            |> List.collect (fun itemStack ->
                itemStack.item.itemEffectSet
                |> Set.toList
                |> List.collect (fun effect ->
                    match effect with
                    | WeaponResource weaponResource -> [ weaponResource ]
                    | _ -> []))

        equipmentList
        |> List.collect (fun itemStack ->

            itemStack.item.itemEffectSet
            |> Set.toList
            |> List.collect (fun effect ->
                match effect with
                | Weapon weapon -> (weapon, itemStack.item.itemTier) |> List.singleton
                | _ -> List.empty))
        |> List.collect (fun (weapon, itemTier) ->

            match weapon.resourceNameOption with
            | Some resourceClass ->
                weaponResourceList
                |> List.collect (fun weaponResource ->
                    if weaponResource.resourceName = resourceClass then
                        (weapon, Some weaponResource, itemTier) |> List.singleton
                    else
                        List.empty)

            | None -> [ (weapon, None, itemTier) ])
        |> List.collect (fun (weapon, weaponResourceOption, itemTier) ->
            let skillDiceModList =
                weaponSkillList
                |> List.tryFind (fun weaponSkill -> weaponSkill.skill.name = weapon.governingSkillName)
                |> (fun weaponSkillOption ->
                    match weaponSkillOption with
                    | Some weaponSkill -> weaponSkill.skill.dicePoolModList
                    | None -> [])

            createCombatRoll weapon itemTier.baseDice skillDiceModList weaponResourceOption)

module Character =
    open AttributeName
    open CoreSkill
    open AttributeAndCoreSkills
    open Vocation
    open DicePoolCalculation
    open DicePoolMod
    open ItemStack
    open CombatRoll

    type Character = {
        name: string
        attributeAndCoreSkillsList: AttributeAndCoreSkills list
        vocationList: Vocation list
        equipmentList: ItemStack list
        combatRollList: CombatRoll List
    }

    let characterToDicePoolCalculation (character: Character) =

        {
            attributeSet =
                List.map
                    (fun attributeAndCoreSkills -> attributeAndCoreSkills.attributeStat)
                    character.attributeAndCoreSkillsList
                |> Set.ofList
            injuryDicePenalty = 0u
            weightClassDicePenalty = 0u
            itemEffectDicePoolMod = createD6DicePoolMod 0u
        }

    let defaultAttributeAndCoreSkillsList (attributeList: AttributeName Set) (coreSkillList: CoreSkill list) =
        Set.map (defaultAttributeAndCoreSkills coreSkillList) attributeList
        |> List.ofSeq