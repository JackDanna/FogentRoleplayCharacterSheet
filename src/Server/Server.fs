module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

module FogentRoleplayServerData =
    open FSharp.Data

    open FogentRoleplayLib.DamageType
    open FogentRoleplayLib.EngageableOpponents
    open FogentRoleplayLib.Range
    open FogentRoleplayLib.ResourceName
    open FogentRoleplayLib.MagicSkillData
    open FogentRoleplayLib.Weapon
    open FogentRoleplayLib.DicePoolMod
    open FogentRoleplayLib.AreaOfEffect

    open FogentRoleplayLib.TypeUtils
    open FogentRoleplayLib.AttributeName
    open FogentRoleplayLib.Neg1To5
    open FogentRoleplayLib.DicePool
    open FogentRoleplayLib.Skill
    open FogentRoleplayLib.CoreSkill

    open FogentRoleplayLib.Container
    open FogentRoleplayLib.WeaponResource
    open FogentRoleplayLib.ItemTier

    let makeFogentRoleplayDataPath fileName =
        __SOURCE_DIRECTORY__ + "../../../FogentRoleplayData/" + fileName

    let makeFogentRoleplayData fileName mappingFunc =
        CsvFile.Load(makeFogentRoleplayDataPath fileName, hasHeaders = true).Rows
        |> Seq.map (mappingFunc)
        |> List.ofSeq

    let Bool boolString =
        match boolString with
        | "TRUE" -> true
        | "FALSE" -> false
        | _ -> failwith ("Error: returns " + boolString)

    // DamageType
    let damageTypeSet =
        makeFogentRoleplayData "DamageTypeData.csv" (fun row -> (DamageType row.["name"]))
        |> Set.ofList

    let stringToDamageTypeSet =
        damageTypeSet |> stringSetToTypeMap |> mapAndStringToDamageTypeSet

    // EngageableOpponents
    let engageableOpponentsCalculationData =
        makeFogentRoleplayData "EngageableOpponentsCalculationData.csv" (fun row -> {
            name = string row.["name"]
            combatRollDivisor = uint row.["combatRollDivisor"]
            maxEO = parseMaxEngageableOpponentsString row.["maxEO"]
        })

    let engageableOpponentsMap =
        parseEngaeableOpponentsString (eoCalculationListToMap engageableOpponentsCalculationData)

    // Range
    let calculatedRangeData =
        makeFogentRoleplayData "CalculatedRangeData.csv" (fun row -> {
            name = string row.["name"]
            effectiveRange = uint row.["effectiveRange"]
            maxRange = uint row.["maxRange"]
        })

    let rangeCalculationData =
        makeFogentRoleplayData "RangeCalculationData.csv" (fun row -> {
            name = string row.["name"]
            numDicePerEffectiveRangeUnit = uint row.["numDicePerEffectiveRangeUnit"]
            ftPerEffectiveRangeUnit = uint row.["ftPerEffectiveRangeUnit"]
            roundEffectiveRangeUp = Bool row.["roundEffectiveRangeUp"]
            maxRange = uint row.["maxRange"]
        })

    let rangeMap = createRangeMap calculatedRangeData rangeCalculationData

    let rangeOptionMap string =
        match string with
        | "None" -> None
        | _ -> rangeMap.Item string |> Some

    // ResourceClass
    let resourceClassData =
        makeFogentRoleplayData "ResourceClassData.csv" (fun row -> (ResourceName row.["name"]))
        |> Set.ofList

    let resourceClassMap = stringSetToTypeMap resourceClassData

    let resourceClassOptionMap string =
        match string with
        | "None" -> None
        | _ -> Some <| resourceClassMap.Item string

    // AttributeAndCoreSkill
    let attributeData: AttributeName Set =
        makeFogentRoleplayData "AttributeData.csv" (fun row -> AttributeName row.["desc"])
        |> Set.ofList

    let coreSkillData: CoreSkill list =
        makeFogentRoleplayData "CoreSkillData.csv" (fun row -> {
            skill = {
                name = row.["name"]
                level = Zero
                dicePool = baseDicePool
            }
            governingAttributeName = row.["governingAttribute"]
        })

    let attributeMap = stringSetToTypeMap attributeData

    let mapAndStringToAttributes (attributeMap: Map<string, AttributeName>) (input) =
        String.filter ((<>) ' ') input
        |> (fun s -> s.Split(',', System.StringSplitOptions.RemoveEmptyEntries))
        |> List.ofArray
        |> List.map (fun attributeString -> attributeMap.Item attributeString)

    let stringToAttributes = mapAndStringToAttributes attributeMap

    //MagicSkillData
    let magicSkillDataSet: MagicSkillData Set =
        makeFogentRoleplayData "MagicSkillData.csv" (fun row -> {
            name = string row.["name"]
            damageTypes = stringToDamageTypeSet (string row.["damageTypes"])
            isMeleeCapable = Bool row.["meleeCapable"]
            isRangeCapable = Bool row.["rangeCapable"]
            magicResource = string row.["magicResourceClass"]
        })
        |> Set.ofList

    let magicSkillMap =
        Set.map (fun (magicSkill: MagicSkillData) -> magicSkill.name, magicSkill) magicSkillDataSet
        |> Map.ofSeq

    // WeaponClass
    let weaponClassData =
        makeFogentRoleplayData "WeaponClassData.csv" (fun row -> {
            name = string row.["name"]
            oneHandedWeaponDice = parseDicePoolModOptionString row.["oneHandedWeaponDice"]
            twoHandedWeaponDice = parseDicePoolModOptionString row.["twoHandedWeaponDice"]
            penetration = uint row.["penetration"]
            range = rangeMap.Item row.["range"]
            damageTypes = stringToDamageTypeSet row.["damageTypes"]
            engageableOpponents = engageableOpponentsMap row.["engageableOpponents"]
            dualWieldableBonus = parseDicePoolModOptionString row.["dualWieldableBonus"]
            areaOfEffect = AreaOfEffectOptionMap.Item row.["areaOfEffect"]
            resourceClass = resourceClassOptionMap row.["resourceClass"]
        })

    let weaponClassMap =
        List.map (fun (weaponClass: Weapon) -> weaponClass.name, weaponClass) weaponClassData
        |> Map.ofList

    // // ConduitClass
    // let conduitClassData =
    //     makeFogentRoleplayData "ConduitClassData.csv" (fun row ->

    //         { name = string row.["desc"]
    //           oneHandedDice = parseDicePoolModOptionString row.["oneHandedDice"]
    //           twoHandedDice = parseDicePoolModString row.["twoHandedDice"]
    //           penetration = uint row.["penetration"]
    //           rangeAdjustment = int row.["rangeAdjustment"]
    //           damageTypes = stringToDamageTypeList row.["damageTypes"]
    //           engageableOpponents =
    //             match row.["engageableOpponents"] with
    //             | "None" -> None
    //             | something -> Some(engageableOpponentsMap something)
    //           dualWieldableBonus = parseDicePoolModOptionString row.["dualWieldableBonus"]
    //           areaOfEffect = AreaOfEffectOptionMap.Item row.["areaOfEffect"]
    //           resourceClass = resourceClassOptionMap row.["resourceClass"]
    //           effectedMagicSkills =
    //             row.["effectedMagicSkills"].Split ", "
    //             |> List.ofArray
    //             |> List.map (fun magicSkillStr -> magicSkillMap.Item magicSkillStr) })

    // let conduitClassMap =
    //     List.map (fun (conduitClass: ConduitClass) -> conduitClass.name, conduitClass) conduitClassData
    //     |> Map.ofList

    // Container
    let containerSet =
        makeFogentRoleplayData "ContainerClassData.csv" (fun row -> {
            name = string row.["Name"]
            weightCapacity = float row.["Weight Capacity"]
            volumeFtCubed = float row.["Volume"]
        })
        |> Set.ofList

    let containerClassMap =
        Set.map (fun (containerClass: Container) -> containerClass.name, containerClass) containerSet
        |> Map.ofSeq

    // WeaponResource
    let weaponResourceClassSet: WeaponResource Set =
        makeFogentRoleplayData "WeaponResourceClassData.csv" (fun row -> {
            name = string row.["desc"]
            resourceName = resourceClassMap.Item row.["resourceClass"]
            dicePoolMod = parseDicePoolModString row.["resourceDice"]
            penetration = uint row.["penetration"]
            rangeOption = rangeOptionMap row.["range"]
            damageTypeSet = stringToDamageTypeSet row.["damageTypes"]
            areaOfEffectOption = AreaOfEffectOptionMap.Item row.["areaOfEffect"]
        })
        |> Set.ofList

    let weaponResourceMap =
        Set.map
            (fun (weaponResourceClass: WeaponResource) -> weaponResourceClass.name, weaponResourceClass)
            weaponResourceClassSet
        |> Map.ofSeq

    // ItemTier
    let itemTierData =
        makeFogentRoleplayData "ItemTierData.csv" (fun row -> {
            name = string row.["desc"]
            level = int row.["level"]
            baseDice = parseDicePoolString row.["baseDice"]
            durabilityMax = uint row.["durabilityMax"]
        })
        |> Set.ofList

    let itemTierMap =
        Set.map (fun (itemTier: ItemTier) -> itemTier.name, itemTier) itemTierData
        |> Map.ofSeq

// // DefenseClass
// let physicalDefenseEffectData: PhysicalDefenseEffect list =
//     makeFogentRoleplayData "PhysicalDefenseEffect.csv" (fun row ->
//         { name = string row.["desc"]
//           physicalDefense = float row.["physicalDefense"] })

// let physicalDefenseEffectMap =
//     physicalDefenseEffectData
//     |> List.map (fun (defenseClass: PhysicalDefenseEffect) -> defenseClass.name, defenseClass)
//     |> Map.ofList

// // SkillDiceModEffect
// let skillDiceModEffectData: SkillDiceModEffect list =
//     makeFogentRoleplayData "SkillDiceModEffect.csv" (fun row ->
//         { name = string row.["Name"]
//           skillToEffect = string row.["Skill"]
//           diceMod = parseDicePoolModString row.["Dice Mod"] })

// let skillDiceModEffectMap =
//     skillDiceModEffectData
//     |> List.map (fun (skillAdjustment: SkillDiceModEffect) -> skillAdjustment.name, skillAdjustment)
//     |> Map.ofList

// // AttributeStatAdjustmentEffect
// let attributeStatAdjustmentEffectData =
//     makeFogentRoleplayData "AttributeStatAdjustmentEffect.csv" (fun row ->
//         { name = string row.["Name"]
//           attribute = AttributeName row.["Attribute"]
//           adjustment = int row.["Adjustment"] })

// let attributeStatAdjustmentEffectMap =
//     attributeStatAdjustmentEffectData
//     |> List.map (fun (attributeStatAdjustmentEffect: AttributeStatAdjustmentEffect) ->
//         attributeStatAdjustmentEffect.name, attributeStatAdjustmentEffect)
//     |> Map.ofList

// // AttributeDeterminedDiceModEffect
// let attributeDeterminedDiceModEffectData =
//     makeFogentRoleplayData "AttributeDeterminedDiceModEffectData.csv" (fun row ->
//         { name = row.["name"]
//           attributesToEffect = stringToAttributes row.["attributesToEffect"]
//           dicePoolMod = parseDicePoolModString row.["dicePoolMod"] })

// let attributeDeterminedDiceModEffectMap =
//     attributeDeterminedDiceModEffectData
//     |> List.map (fun (attributeDeterminedDiceModEffect: AttributeDeterminedDiceModEffect) ->
//         attributeDeterminedDiceModEffect.name, attributeDeterminedDiceModEffect)
//     |> Map.ofList

// // WeightClass
// let weightClassData: WeightClass list =
//     makeFogentRoleplayData "WeightClassData.csv" (fun row ->
//         { name = row.["name"]
//           bottomPercent = float row.["bottomPercent"]
//           topPercent = float row.["topPercent"]
//           attributeDeterminedDiceModEffect =
//             attributeDeterminedDiceModEffectMap.Item row.["attributeDeterminedDiceModEffect"] })

// // MovementSpeedCalculation
// let movementSpeedCalculationData =
//     makeFogentRoleplayData "MovementSpeedCalculationData.csv" (fun row ->
//         { name = string row.["desc"]
//           baseMovementSpeed = uint row.["baseMovementSpeed"]
//           governingAttribute = attributeMap.Item row.["governingAttributes"]
//           feetPerAttributeLvl = uint row.["feetPerAttributeLvl"]
//           governingSkill = string row.["governingSkill"]
//           feetPerSkillLvl = uint row.["feetPerSkillLvl"] })

// let movementSpeedCalculationMap =
//     movementSpeedCalculationData
//     |> List.map (fun movementSpeedCalculationData -> movementSpeedCalculationData.name, movementSpeedCalculationData)
//     |> Map.ofList

// // CarryWeightCalculation
// let carryWeightCalculationData =
//     makeFogentRoleplayData "CarryWeightCalculationData.csv" (fun row ->
//         { name = string row.["name"]
//           baseWeight = uint row.["baseWeight"]
//           governingAttribute = AttributeName row.["governingAttribute"]
//           weightIncreasePerAttribute = uint row.["weightIncreasePerAttribute"]
//           governingSkill = string row.["governingSkill"]
//           weightIncreasePerSkill = uint row.["weightIncreasePerSkill"] })

// let carryWeightCalculationMap =
//     carryWeightCalculationData
//     |> List.map (fun carryWeightCalculation -> carryWeightCalculation.name, carryWeightCalculation)
//     |> Map.ofList

// // Effect
// let effectData: Effect list =
//     List.map SkillDiceModEffect skillDiceModEffectData
//     @ List.map AttributeStatAdjustmentEffect attributeStatAdjustmentEffectData
//       @ List.map PhysicalDefenseEffect physicalDefenseEffectData
//         @ List.map AttributeDeterminedDiceModEffect attributeDeterminedDiceModEffectData
//           @ List.map MovementSpeedCalculation movementSpeedCalculationData

// let effectDataMap =
//     effectData
//     |> List.map (fun (effect: Effect) -> effectToEffectName effect, effect)
//     |> Map.ofList

// // TextEffectForDisplay
// let textEffectForDisplayData: TextEffectForDisplay list =
//     makeFogentRoleplayData "CharacterEffectForDisplayData.csv" (fun row ->
//         { name = string row.["Name"]
//           effect = string row.["Effect"]
//           durationAndSource =
//             { duration = string row.["Duration"]
//               source = string row.["Source"] } })

// // EffectForDisplay
// let effectForDisplayData: EffectForDisplay list =
//     let skillDiceModEffectForDisplayList =
//         List.map skillDiceModEffectToSkillDiceModEffectForDisplay skillDiceModEffectData

//     let attributeDeterminedDiceModEffectForDisplayList =
//         List.map attributeDeterminedDiceModEffectToForDisplay attributeDeterminedDiceModEffectData

//     List.map TextEffectForDisplay textEffectForDisplayData
//     @ List.map SkillDiceModEffectForDisplay skillDiceModEffectForDisplayList
//       @ List.map AttributeDeterminedDiceModEffectForDisplay attributeDeterminedDiceModEffectForDisplayList

// let effectForDisplayMap: Map<string, EffectForDisplay> =
//     effectForDisplayData
//     |> List.map (fun (characterEffect: EffectForDisplay) -> effectForDiplayToName characterEffect, characterEffect)
//     |> Map.ofList



// // Item
// let stringToItemClassList
//     (weaponClassMap: Map<string, WeaponClass>)
//     (conduitClassMap: Map<string, ConduitClass>)
//     (weaponResourceClassMap: Map<string, WeaponResourceClass>)
//     (input: string)
//     =
//     input.Split ", "
//     |> List.ofArray
//     |> List.collect (fun className ->
//         match className with
//         | weaponClassName when weaponClassMap.Keys.Contains weaponClassName ->
//             weaponClassMap.Item weaponClassName
//             |> WeaponClass
//             |> List.singleton
//         | conduitClassName when conduitClassMap.Keys.Contains conduitClassName ->
//             conduitClassMap.Item conduitClassName
//             |> ConduitClass
//             |> List.singleton
//         | weaponResourceClassName when weaponResourceClassMap.Keys.Contains weaponResourceClassName ->
//             weaponResourceClassMap.Item weaponResourceClassName
//             |> WeaponResourceClass
//             |> List.singleton
//         | containerClassName when containerClassMap.Keys.Contains containerClassName ->
//             containerClassMap.Item containerClassName
//             |> ContainerClass
//             |> List.singleton
//         | itemEffectName when effectDataMap.Keys.Contains itemEffectName ->
//             effectDataMap.Item itemEffectName
//             |> ItemEffect
//             |> List.singleton
//         | _ -> [])

// let itemStackData =
//     makeFogentRoleplayData "ItemData.csv" (fun row ->
//         { quantity = uint row.["quantity"]
//           item =
//             { name = string row.["desc"]
//               itemClasses =
//                 stringToItemClassList weaponClassMap conduitClassMap weaponResourceClassMap row.["itemClasses"]
//               itemTier = itemTierMap.Item row.["itemTier"]
//               value = string row.["value"]
//               weight = float row.["weight"] } })

// // CombatVocationSkills

// let combatVocationalSkill =
//     List.append
//         (List.map (fun (weaponClassData: WeaponClass) -> weaponClassData.name) weaponClassData)
//         (List.map (fun (magicSkill: MagicSkill) -> magicSkill.name) magicSkillData)

let fallenDataApi: IFogentRoleplayDataApi = {
    getInitData =
        fun () -> async {
            return {
                defaultCoreSkillList = FogentRoleplayServerData.coreSkillData
                defaultAttributeSet = FogentRoleplayServerData.attributeData
            //   allItemStackList = FallenServerData.itemStackData
            //   magicSkillMap = FallenServerData.magicSkillMap
            //   magicCombatMap = FallenServerData.magicCombatMap
            //   rangeMap = FallenServerData.rangeMap
            //   combatVocationalSkill = FallenServerData.combatVocationalSkill
            //   effectForDisplayMap = FallenServerData.effectForDisplayMap
            //   carryWeightCalculationMap = FallenServerData.carryWeightCalculationMap
            //   weightClassList = FallenServerData.weightClassData
            //   movementSpeedCalculationMap = FallenServerData.movementSpeedCalculationMap
            }
        }
}

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue fallenDataApi
    |> Remoting.buildHttpHandler

let app = application {
    use_router webApp
    memory_cache
    use_static "public"
    use_gzip
}

[<EntryPoint>]
let main _ =
    run app
    0