module CsvDatabase


open FSharp.Data
open FogentRoleplayLib.StringUtils
open FogentRoleplayLib.DamageType
open FogentRoleplayLib.EngageableOpponents
open FogentRoleplayLib.RangeCalculation
open FogentRoleplayLib.CalculatedRange
open FogentRoleplayLib.Range
open FogentRoleplayLib.ResourceName
open FogentRoleplayLib.Weapon
open FogentRoleplayLib.DicePoolMod
open FogentRoleplayLib.TypeUtils
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.Container
open FogentRoleplayLib.WeaponResource
open FogentRoleplayLib.PhysicalDefense
open FogentRoleplayLib.SkillDiceMod
open FogentRoleplayLib.SetSphere
open FogentRoleplayLib.SetCone
open FogentRoleplayLib.SetAreaOfEffect
open FogentRoleplayLib.AreaOfEffect
open FogentRoleplayLib.SphereCalculation
open FogentRoleplayLib.ConeCalculation
open FogentRoleplayLib.AreaOfEffectCalculation
open FogentRoleplayLib.AttributeDeterminedDiceMod
open FogentRoleplayLib.WeightClass
open FogentRoleplayLib.Effect
open FogentRoleplayLib.AttributeStatAdjustment
open FogentRoleplayLib.ItemStack
open FogentRoleplayLib.WeaponSpell
open FogentRoleplayLib.MagicSystem
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.SkillName
open FogentRoleplayLib.WeaponSkillData
open FogentRoleplayLib.ParsingUtils
open FogentRoleplayLib.TextEffect
open FogentRoleplayLib.MagicSkillData
open FogentRoleplayLib.CoreSkillData

open DatabaseUtils

let makeFogentRoleplayDataPath fileName =
    __SOURCE_DIRECTORY__ + "../../../FogentRoleplayData/" + fileName

let makeFogentRoleplayDataSeq fileName mappingFunc =
    CsvFile.Load(makeFogentRoleplayDataPath fileName, hasHeaders = true).Rows
    |> Seq.map (mappingFunc)

let makeFogentRoleplayDataSeqExcludingFileExtionsion fileNameWithoutExtension mappingFunc =
    makeFogentRoleplayDataSeq (tableNameToCSVFileName fileNameWithoutExtension) mappingFunc

let makeFogentRoleplayDataListExcludingFileExtension fileNameWithoutExtension mappingFunc =
    makeFogentRoleplayDataSeqExcludingFileExtionsion fileNameWithoutExtension mappingFunc
    |> List.ofSeq

let makeFogentRoleplayDataSetExcludingFileExtension fileNameWithoutExtension mappingFunc =
    makeFogentRoleplayDataSeqExcludingFileExtionsion fileNameWithoutExtension mappingFunc
    |> Set.ofSeq

let Bool boolString =
    match boolString with
    | "TRUE" -> true
    | "FALSE" -> false
    | _ -> failwith ("Error: returns " + boolString)

// DamageType

let damageTypes =
    makeFogentRoleplayDataSetExcludingFileExtension damagageTypeTableName (fun row -> (DamageType row.["name"]))

let stringToDamageTypeSet =
    damageTypes |> stringSetToTypeMap |> mapAndStringToValueSet

// EngageableOpponents

let engageableOpponentsCalculations: Set<EngageableOpponentsCalculation> =
    makeFogentRoleplayDataSetExcludingFileExtension engageableOpponentsCalculationTableName (fun row -> {
        name = string row.["name"]
        combatRollDivisor = uint row.["combatRollDivisor"]
        maxEOOption = parseMaxEngageableOpponentsString row.["maxEO"]
    })

let engageableOpponentsMap =
    engageableOpponentsCalculations
    |> eoCalculationSetToMap
    |> parseEngaeableOpponentsString

// Range Utilities
let parseMaxRangeOption input =
    match input with
    | "" -> None
    | validInput -> Some(uint validInput)

// CalculatedRange

let calculatedRanges: CalculatedRange list =
    makeFogentRoleplayDataListExcludingFileExtension calculatedRangeTableName (fun row -> {
        name = string row.["name"]
        effectiveRange = uint row.["effectiveRange"]
        maxRangeOption = parseMaxRangeOption row.["maxRangeOption"]
    })

// RangeCalculation

let rangeCalculations =
    makeFogentRoleplayDataListExcludingFileExtension rangeCalculationTableName (fun row -> {
        name = string row.["name"]
        numDicePerEffectiveRangeUnit = uint row.["numDicePerEffectiveRangeUnit"]
        ftPerEffectiveRangeUnit = uint row.["ftPerEffectiveRangeUnit"]
        roundEffectiveRangeUp = Bool row.["roundEffectiveRangeUp"]
        maxRangeOption = parseMaxRangeOption row.["maxRangeOption"]
    })

// Range

let rangeMap = (calculatedRanges, rangeCalculations) ||> createRangeMap

let rangeOptionMap string =
    match string with
    | "None" -> None
    | _ -> rangeMap.Item string |> Some

// SphereCalculation

let sphereCalculationSet =
    makeFogentRoleplayDataSetExcludingFileExtension $"AreaOfEffects/{sphereCalculationTableName}" (fun row -> {
        name = string row.["name"]
        initRadius = float row.["Init Radius"]
        radiusPerDice = float row.["Radius per Dice"]
    })

// ConeCalculation

let coneCalculationSet =

    makeFogentRoleplayDataSetExcludingFileExtension $"AreaOfEffects/{coneCalculationTableName}" (fun row -> {
        name = string row.["name"]
        angle = float row.["angle"]
        initBaseAndHeight = float row.["initBaseAndHeight"]
        baseAndHeightPerDice = float row.["base/height per unit"]
    })

// SetSphere

let setSphereSet =
    makeFogentRoleplayDataSetExcludingFileExtension $"AreaOfEffects/{setSphereTableName}" (fun row -> {
        name = row.["Name"]
        radius = uint row.["Radius(ft)"]
    })

// SetCone

let setConeSet =
    makeFogentRoleplayDataSetExcludingFileExtension $"AreaOfEffects/{setConeTableName}" (fun row -> {
        name = string row.["name"]
        baseAndHeight = uint row.["Triangle Base/Height (ft)"]
        angle = float row.["Cone Angle (degrees)"]
    })

let areaOfEffectMap =
    [
        Set.map (fun (setCone: SetCone) -> (setCone.name, setCone |> SetCone |> SetAreaOfEffect)) setConeSet
        Set.map (fun (setSphere: SetSphere) -> (setSphere.name, setSphere |> SetSphere |> SetAreaOfEffect)) setSphereSet
        Set.map
            (fun (coneCalculation: ConeCalculation) ->
                (coneCalculation.name, coneCalculation |> ConeCalculation |> AreaOfEffectCalculation))
            coneCalculationSet
        Set.map
            (fun (sphereCalculation: SphereCalculation) ->
                (sphereCalculation.name, sphereCalculation |> SphereCalculation |> AreaOfEffectCalculation))
            sphereCalculationSet
    ]
    |> Set.unionMany
    |> Map.ofSeq


let namedAreaOfEffectOptionMap key =
    match key with
    | "" -> None
    | _ -> areaOfEffectMap.Item key |> Some

// ResourceClass
let resourceMap =
    makeFogentRoleplayDataSetExcludingFileExtension resourceNameTableName (fun row -> (ResourceName row.["name"]))
    |> stringSetToTypeMap

let resourceOptionMap string =
    match string with
    | "None" -> None
    | _ -> resourceMap.Item string |> Some

// AttributeAndCoreSkill

let coreSkillDataSet: Set<CoreSkillData> =
    makeFogentRoleplayDataSetExcludingFileExtension coreSkillDataTableName (fun row -> {
        skillName = SkillName row.["name"]
        attributeName = AttributeName row.["governingAttribute"]
    })

let attributeNameSet =
    coreSkillDataSet |> Set.map (fun coreSkillData -> coreSkillData.attributeName)

let attributeNameMap = stringSetToTypeMap attributeNameSet

let stringToAttributes = mapAndStringToValueSet attributeNameMap

//MagicSkillData
let magicSkillDataSet: Set<MagicSkillData> =
    makeFogentRoleplayDataSetExcludingFileExtension magicSkillTableName (fun row -> {
        name = SkillName row.["name"]
        damageTypes = stringToDamageTypeSet (string row.["damageTypes"])
        isMeleeCapable = Bool row.["meleeCapable"]
        isRangeCapable = Bool row.["rangeCapable"]
    })

let magicSkillDataMap = makeMagicSkillDataMap magicSkillDataSet


// MagicSystem
let magicSystemSet =
    makeFogentRoleplayDataSetExcludingFileExtension magicSystemTableName (fun row -> {
        name = row.["name"]
        vocationName = row.["vocationName"]
        vocationGoverningAttributeSet = stringToAttributes row.["vocationGoverningAttributeSet"]
        resourceName = row.["resourceName"]
        governingCoreSkill = row.["governingCoreSkill"]
        magicSkillDataSet =
            row.["magicSkillNameSet"]
            |> commaSeperatedStringToSet
            |> Set.map (fun key -> magicSkillDataMap.Item key)
    })

// BaseDiceTiers
open FogentRoleplayLib.BaseDiceTier

let baseDiceTiers =
    makeFogentRoleplayDataSetExcludingFileExtension baseDiceTierTableName (fun row -> {
        itemPrefix = string row.["itemPrefix"]
        level = int row.["level"]
        baseDice = parseDicePoolString row.["baseDice"]
    //itemDurabilityMax = uint row.["itemDurabilityMax"]
    })

// WeaponClass
let weaponSet =
    baseDiceTiers
    |> Set.map (fun (baseDiceTier: BaseDiceTier) ->
        let createPrefixedWeaponClasses prefix =
            makeFogentRoleplayDataSetExcludingFileExtension weaponClassTableName (fun row -> {
                name = $"""{prefix} {row.["name"]}"""
                governingSkillName = SkillName row.["governingSkillName"]
                oneHandedDiceMod = parseDicePoolModOptionString row.["oneHandedWeaponDice"]
                twoHandedDiceMod = parseDicePoolModOptionString row.["twoHandedWeaponDice"]
                penetration = uint row.["penetration"]
                range = rangeMap.Item row.["range"]
                damageTypes = stringToDamageTypeSet row.["damageTypes"]
                engageableOpponents = engageableOpponentsMap row.["engageableOpponents"]
                dualWieldedDiceMod = parseDicePoolModOptionString row.["dualWieldableBonus"]
                areaOfEffectOption = namedAreaOfEffectOptionMap row.["areaOfEffect"]
                resourceNameOption = resourceOptionMap row.["resourceClass"]
                baseDiceTier = baseDiceTier
            })

        if baseDiceTier.itemPrefix = "Mundane" then
            Set.union (createPrefixedWeaponClasses baseDiceTier.itemPrefix) (createPrefixedWeaponClasses "")
        else
            createPrefixedWeaponClasses baseDiceTier.itemPrefix)
    |> Set.intersectMany

// WeaponSkillData
let weaponSkillDataSet =
    makeFogentRoleplayDataSetExcludingFileExtension weaponSkillTableName (fun row -> {
        name = string row.["skillName"]
        governingAttributes = stringToAttributes row.["governingAttributes"]
    })

let weaponSkillDataMap = makeWeaponSkillDataMap weaponSkillDataSet

// WeaponSpell
let weaponSpellSet: WeaponSpell Set =
    makeFogentRoleplayDataSetExcludingFileExtension weaponSpellTableName (fun row -> {
        name = string row.["name"]
        oneHandedDiceMod = parseDicePoolModOptionString row.["oneHandedWeaponDice"]
        twoHandedDiceMod = parseDicePoolModOptionString row.["twoHandedWeaponDice"]
        dualWieldedDiceMod = parseDicePoolModOptionString row.["dualWieldableBonus"]
        penetration = uint row.["penetration"]
        range = rangeMap.Item row.["range"]
        engageableOpponents = engageableOpponentsMap row.["engageableOpponents"]
        areaOfEffectOption = namedAreaOfEffectOptionMap row.["areaOfEffect"]
        magicResourceAmount = uint row.["magicResourceAmount"]
    })

// Container
let containerMap =
    makeFogentRoleplayDataSetExcludingFileExtension containerTableName (fun row -> {
        name = string row.["Name"]
        weightCapacity = float row.["Weight Capacity"]
        volumeFtCubed = float row.["Volume"]
    })
    |> Set.map (fun containerMap -> containerMap.name, containerMap)
    |> Map.ofSeq


//WeaponResource
let weaponResourceSet =
    makeFogentRoleplayDataSetExcludingFileExtension weaponResourceTableName (fun row -> {
        name = string row.["desc"]
        resourceName = resourceMap.Item row.["resourceClass"]
        dicePoolMod = parseDicePoolModString row.["resourceDice"]
        penetration = uint row.["penetration"]
        rangeOption = rangeOptionMap row.["range"]
        damageTypeSet = stringToDamageTypeSet row.["damageTypes"]
        namedAreaOfEffectOption = namedAreaOfEffectOptionMap row.["areaOfEffect"]
    })

// PhysicalDefense
let physicalDefenseSet =
    makeFogentRoleplayDataSetExcludingFileExtension physicalDefenseTableName (fun row -> {
        name = string row.["name"]
        physicalDefense = float row.["physicalDefense"]
        durationAndSource = {
            duration = row.["duration"]
            source = row.["source"]
        }
    })

// SkillDiceModEffect
let skillDiceModEffectSet =
    makeFogentRoleplayDataSetExcludingFileExtension skillDiceModTableName (fun row -> {
        name = string row.["Name"]
        skillToEffect = string row.["Skill"]
        diceMod = parseDicePoolModString row.["Dice Mod"]
        durationAndSource = {
            duration = row.["duration"]
            source = row.["source"]
        }
    })

// // AttributeStatAdjustmentEffect
let attributeStatAdjustmentEffectData =
    makeFogentRoleplayDataSetExcludingFileExtension $"Effect/{attributeStatAdjustmentTableName}" (fun row -> {
        name = string row.["Name"]
        attribute = AttributeName row.["Attribute"]
        adjustment = int row.["Adjustment"]
        durationAndSource = {
            duration = row.["duration"]
            source = row.["source"]
        }
    })

// AttributeDeterminedDiceModEffect
let attributeDeterminedDiceModSet =
    makeFogentRoleplayDataSetExcludingFileExtension $"Effect/{attributeDeterminedDiceModTableName}" (fun row -> {
        name = row.["name"]
        attributesToEffect = stringToAttributes row.["attributesToEffect"]
        dicePoolMod = parseDicePoolModString row.["dicePoolMod"]
        durationAndSource = {
            duration = row.["duration"]
            source = row.["source"]
        }
    })

let attributeDeterminedDiceModMap =
    attributeDeterminedDiceModSet
    |> Set.map (fun (attributeDeterminedDiceModEffect: AttributeDeterminedDiceMod) ->
        attributeDeterminedDiceModEffect.name, attributeDeterminedDiceModEffect)
    |> Map.ofSeq

// BaseDiceModEffect
open FogentRoleplayLib.BaseDiceMod

let weaponSkillBaseDiceMods =
    weaponSkillDataMap.Values
    |> Seq.collect (fun weaponSkillData ->
        baseDiceTiers
        |> Set.map (fun baseDiceTier -> (weaponSkillData, baseDiceTier))
        |> Set.toSeq)
    |> Seq.map (fun (weaponSkill: WeaponSkillData, baseDiceTier: BaseDiceTier) -> {
        name = baseDiceTier.itemPrefix + " " + weaponSkill.name
        effectedSkillName = weaponSkill.name
        baseDiceTier = baseDiceTier
        durationAndSource = { duration = ""; source = "" }
    })
    |> Set.ofSeq

// Need to make baseDiceMods for magic skill and core skills
let magicSkillBaseDiceMods = Set.empty

let coreSkillBaseDiceMods = Set.empty

let baseDiceModSet =
    Set.unionMany [ weaponSkillBaseDiceMods; magicSkillBaseDiceMods; coreSkillBaseDiceMods ]

// WeightClass
let weightClassSet: WeightClass Set =
    makeFogentRoleplayDataSetExcludingFileExtension weightClassTableName (fun row -> {
        name = row.["name"]
        bottomPercentOption =
            if "" = row.["bottomPercent"] then
                None
            else
                Some(float row.["bottomPercent"])
        topPercentOption =
            if "" = row.["topPercent"] then
                None
            else
                Some(float row.["topPercent"])
        attributeDeterminedDiceModEffect = attributeDeterminedDiceModMap.Item row.["attributeDeterminedDiceModEffect"]
    })

// Combat Speed
open FogentRoleplayLib.SpeedCalculation
open FogentRoleplayLib.CombatSpeedCalculation

let speedMap: Map<string, SpeedCalculation> =
    makeFogentRoleplayDataSetExcludingFileExtension speedCalculationTableName (fun row -> {
        name = row.["name"]
        feetPerGoverningSkillDice = float row.["feetPerGoverningSkillDice"]
        feetPerReactionSpeedAttribute = float row.["feetPerReactionSpeedAttribute"]
    })
    |> Set.map (fun speed -> (speed.name, speed))
    |> Map.ofSeq

let combatSpeedCalculationSet =
    makeFogentRoleplayDataSetExcludingFileExtension combatSpeedCalculationTableName (fun row -> {
        name = row.["name"]
        governingSkillName = SkillName row.["governingSkillName"]
        reactionSpeedAttributeName = AttributeName row.["reactionSpeedAttributeName"]
        speed = speedMap.Item row.["speed"]
    })

// CarryWeightCalculation
open FogentRoleplayLib.CarryWeightCalculation

let carryWeightCalculationSet =
    makeFogentRoleplayDataSetExcludingFileExtension carryWeightCalculationTableName (fun row -> {
        name = string row.["name"]
        baseWeight = uint row.["baseWeight"]
        governingAttribute = AttributeName row.["governingAttribute"]
        weightIncreasePerAttribute = uint row.["weightIncreasePerAttribute"]
        governingSkill = string row.["governingSkill"]
        weightIncreasePerSkill = uint row.["weightIncreasePerSkill"]
    })

// TextEffectForDisplay
let textEffect: TextEffect Set =
    makeFogentRoleplayDataSetExcludingFileExtension $"Effect/{textEffectTableName}" (fun row -> {
        name = string row.["Name"]
        effect = string row.["Effect"]
        durationAndSource = {
            duration = row.["duration"]
            source = row.["source"]
        }
    })

// Effect
let effectDataSet =
    [
        Set.map Weapon weaponSet
        Set.map WeaponResource weaponResourceSet
        Set.map SkillDiceMod skillDiceModEffectSet
        Set.map AttributeStatAdjustment attributeStatAdjustmentEffectData
        Set.map PhysicalDefense physicalDefenseSet
        Set.map AttributeDeterminedDiceMod attributeDeterminedDiceModSet
        Set.map BaseDiceMod baseDiceModSet
        Set.map TextEffect textEffect
    ]
    |> Set.unionMany

let effectDataMap = makeEffectDataMap effectDataSet

// Item
let stringToEffectSet (effectMap: Map<string, Effect>) (input: string) =
    input
    |> commaSeperatedStringToSet
    |> Set.filter (fun effectName -> effectMap.Keys.Contains effectName)
    |> Set.map (fun effectName -> effectMap.Item effectName)

open FogentRoleplayLib.ItemElement
open FogentRoleplayLib.Item
open System

let createItemFromRow (row: CsvRow) = {
    name = string row.["desc"]
    itemEffectSet =
        Set.unionMany [
            (stringToEffectSet effectDataMap row.["itemClasses"])
            (stringToEffectSet effectDataMap row.["itemEffects"])
            (stringToEffectSet effectDataMap row.["itemTier"])
        ]
    value = string row.["value"]
    weight = float row.["weight"]
}

let itemElementSet =
    makeFogentRoleplayDataSetExcludingFileExtension itemElementTableName (fun row ->

        match row.["quantity"] with
        | quantity when isNumeric quantity ->
            {
                quantity = uint row.["quantity"]
                item = createItemFromRow row
            }
            |> ItemStack
            |> Some
        | quantity when String.IsNullOrEmpty quantity -> createItemFromRow row |> Item |> Some
        | quantity when containerMap.ContainsKey quantity ->
            {
                containerTypeData = containerMap.Item quantity
                containedElements = []
                item = createItemFromRow row
            }
            |> ContainerItem
            |> Some
        | _ -> None)
    |> Seq.choose id
    |> Set.ofSeq

open FogentRoleplayLib.SettingData

let getInitSettingDataFromCSV () : SettingData = {
    attributeNameSet = attributeNameSet
    coreSkillDataSet = coreSkillDataSet
    itemElementSet = itemElementSet
    weaponSpellSet = weaponSpellSet
    magicSystemSet = magicSystemSet
    weaponSkillDataSet = weaponSkillDataSet
    effectSet = effectDataSet
    combatSpeedCalculationSet = combatSpeedCalculationSet
    carryWeightCalculationSet = carryWeightCalculationSet
    weightClassSet = weightClassSet
}