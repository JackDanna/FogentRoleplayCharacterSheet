module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

module FogentRoleplayServerData =
    open FSharp.Data

    open FogentRoleplayLib.StringUtils

    open FogentRoleplayLib.DamageType
    open FogentRoleplayLib.EngageableOpponents
    open FogentRoleplayLib.Range
    open FogentRoleplayLib.ResourceName
    open FogentRoleplayLib.MagicSkillData
    open FogentRoleplayLib.Weapon
    open FogentRoleplayLib.DicePoolMod

    open FogentRoleplayLib.TypeUtils
    open FogentRoleplayLib.AttributeName
    open FogentRoleplayLib.Neg1To5
    open FogentRoleplayLib.DicePool
    open FogentRoleplayLib.Skill
    open FogentRoleplayLib.CoreSkill

    open FogentRoleplayLib.ContainerClass
    open FogentRoleplayLib.WeaponResource
    open FogentRoleplayLib.ItemTier

    open FogentRoleplayLib.PhysicalDefenseEffect
    open FogentRoleplayLib.SkillDiceModEffect

    open FogentRoleplayLib.SetAreaOfEffect
    open FogentRoleplayLib.AreaOfEffect
    open FogentRoleplayLib.AreaOfEffectCalculation
    open FogentRoleplayLib.AttributeDeterminedDiceModEffect
    open FogentRoleplayLib.WeightClass
    open FogentRoleplayLib.MovementSpeedEffect
    open FogentRoleplayLib.Effect
    open FogentRoleplayLib.AttributeStatAdjustmentEffect
    open FogentRoleplayLib.ItemStack

    let makeFogentRoleplayDataPath fileName =
        __SOURCE_DIRECTORY__ + "../../../FogentRoleplayData/" + fileName

    let makeFogentRoleplayDataList fileName mappingFunc =
        CsvFile.Load(makeFogentRoleplayDataPath fileName, hasHeaders = true).Rows
        |> Seq.map (mappingFunc)
        |> List.ofSeq

    let makeFogentRoleplayDataSet fileName mappingFunc =
        makeFogentRoleplayDataList fileName mappingFunc |> Set.ofList

    let Bool boolString =
        match boolString with
        | "TRUE" -> true
        | "FALSE" -> false
        | _ -> failwith ("Error: returns " + boolString)

    // DamageType
    let stringToDamageTypeSet =
        makeFogentRoleplayDataSet "DamageTypeData.csv" (fun row -> (DamageType row.["name"]))
        |> stringSetToTypeMap
        |> mapAndStringToValueSet

    // EngageableOpponents
    let engageableOpponentsMap =
        makeFogentRoleplayDataSet "EngageableOpponentsCalculationData.csv" (fun row -> {
            name = string row.["name"]
            combatRollDivisor = uint row.["combatRollDivisor"]
            maxEO = parseMaxEngageableOpponentsString row.["maxEO"]
        })
        |> eoCalculationSetToMap
        |> parseEngaeableOpponentsString

    // Range

    let rangeMap =
        (makeFogentRoleplayDataList "CalculatedRangeData.csv" (fun row -> {
            name = string row.["name"]
            effectiveRange = uint row.["effectiveRange"]
            maxRange = uint row.["maxRange"]
         }),
         makeFogentRoleplayDataList "RangeCalculationData.csv" (fun row -> {
             name = string row.["name"]
             numDicePerEffectiveRangeUnit = uint row.["numDicePerEffectiveRangeUnit"]
             ftPerEffectiveRangeUnit = uint row.["ftPerEffectiveRangeUnit"]
             roundEffectiveRangeUp = Bool row.["roundEffectiveRangeUp"]
             maxRange = uint row.["maxRange"]
         }))
        ||> createRangeMap

    let rangeOptionMap string =
        match string with
        | "None" -> None
        | _ -> rangeMap.Item string |> Some

    // AreaOfEffect
    let SetConeSet =
        makeFogentRoleplayDataSet "AreaOfEffects/SetCone.csv" (fun row -> {
            name = string row.["name"]
            baseAndHeight = uint row.["Triangle Base/Height (ft)"]
            angle = float row.["Cone Angle (degrees)"]
        })

    let SetSphereSet =
        makeFogentRoleplayDataSet "AreaOfEffects/SetSphere.csv" (fun row -> {
            name = row.["Name"]
            radius = uint row.["Radius(ft)"]
        })

    let coneCalculationSet =
        makeFogentRoleplayDataSet "AreaOfEffects/ConeCalculation.csv" (fun row -> {
            name = string row.["name"]
            angle = float row.["angle"]
            initBaseAndHeight = float row.["init triangle base/height"]
            baseAndHeightPerDice = float row.["base/height per unit"]
        })

    let sphereCalculationSet =
        makeFogentRoleplayDataSet "AreaOfEffects/SphereCalculation.csv" (fun row -> {
            name = string row.["name"]
            initRadius = float row.["Init Radius"]
            radiusPerDice = float row.["Radius per Dice"]
        })

    let areaOfEffectMap =
        [
            Set.map (fun (setCone: SetCone) -> (setCone.name, setCone |> SetCone |> SetAreaOfEffect)) SetConeSet
            Set.map
                (fun (setSphere: SetSphere) -> (setSphere.name, setSphere |> SetSphere |> SetAreaOfEffect))
                SetSphereSet
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
        makeFogentRoleplayDataSet "ResourceClassData.csv" (fun row -> (ResourceName row.["name"]))
        |> stringSetToTypeMap

    let resourceOptionMap string =
        match string with
        | "None" -> None
        | _ -> resourceMap.Item string |> Some

    // AttributeAndCoreSkill
    let attributeNameSet: AttributeName Set =
        makeFogentRoleplayDataSet "AttributeData.csv" (fun row -> AttributeName row.["desc"])

    let coreSkillData: CoreSkill list =
        makeFogentRoleplayDataList "CoreSkillData.csv" (fun row -> {
            skill = {
                name = row.["name"]
                level = Zero
                baseDice = base3d6DicePool
                dicePoolModList = []
            }
            governingAttributeName = row.["governingAttribute"]
        })

    let attributeNameMap = stringSetToTypeMap attributeNameSet


    let stringToAttributes = mapAndStringToValueSet attributeNameMap

    //MagicSkillData
    let magicSkillMap =
        makeFogentRoleplayDataSet "MagicSkillData.csv" (fun row -> {
            name = string row.["name"]
            damageTypes = stringToDamageTypeSet (string row.["damageTypes"])
            isMeleeCapable = Bool row.["meleeCapable"]
            isRangeCapable = Bool row.["rangeCapable"]
            magicResource = string row.["magicResourceClass"]
        })
        |> Set.map (fun (magicSkill: MagicSkillData) -> magicSkill.name, magicSkill)
        |> Map.ofSeq

    // WeaponClass
    let weaponSet =
        makeFogentRoleplayDataSet "WeaponClassData.csv" (fun row -> {
            name = string row.["name"]
            oneHandedWeaponDice = parseDicePoolModOptionString row.["oneHandedWeaponDice"]
            twoHandedWeaponDice = parseDicePoolModOptionString row.["twoHandedWeaponDice"]
            penetration = uint row.["penetration"]
            range = rangeMap.Item row.["range"]
            damageTypes = stringToDamageTypeSet row.["damageTypes"]
            engageableOpponents = engageableOpponentsMap row.["engageableOpponents"]
            dualWieldableBonus = parseDicePoolModOptionString row.["dualWieldableBonus"]
            areaOfEffectOption = namedAreaOfEffectOptionMap row.["areaOfEffect"]
            resourceNameOption = resourceOptionMap row.["resourceClass"]
            governingSkillName = SkillName row.["governingSkill"]
        })

    let weaponGoverningSkillNameSet: SkillName Set =
        Set.map (_.governingSkillName) weaponSet

    // |> Set.map (fun (weaponClass: Weapon) -> weaponClass.name, weaponClass)
    // |> Map.ofSeq

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
        makeFogentRoleplayDataList "ContainerClassData.csv" (fun row -> {
            name = string row.["Name"]
            weightCapacity = float row.["Weight Capacity"]
            volumeFtCubed = float row.["Volume"]
        })
        |> Set.ofList

    let containerClassMap =
        Set.map (fun (containerClass: ContainerClass) -> containerClass.name, containerClass) containerSet
        |> Map.ofSeq

    //WeaponResource
    let weaponResourceSet =
        makeFogentRoleplayDataSet "WeaponResourceClassData.csv" (fun row -> {
            name = string row.["desc"]
            resourceName = resourceMap.Item row.["resourceClass"]
            dicePoolMod = parseDicePoolModString row.["resourceDice"]
            penetration = uint row.["penetration"]
            rangeOption = rangeOptionMap row.["range"]
            damageTypeSet = stringToDamageTypeSet row.["damageTypes"]
            NamedAreaOfEffectOption = namedAreaOfEffectOptionMap row.["areaOfEffect"]
        })
    // |> Set.map (fun (weaponResource: WeaponResource) -> weaponResource.name, weaponResource)
    // |> Map.ofSeq

    // ItemTier
    let itemTierMap =
        makeFogentRoleplayDataSet "ItemTierData.csv" (fun row -> {
            name = string row.["desc"]
            level = int row.["level"]
            baseDice = parseDicePoolString row.["baseDice"]
            durabilityMax = uint row.["durabilityMax"]
        })
        |> Set.map (fun (itemTier: ItemTier) -> itemTier.name, itemTier)
        |> Map.ofSeq

    // DefenseClass

    let physicalDefenseSet =
        makeFogentRoleplayDataSet "PhysicalDefenseEffect.csv" (fun row -> {
            name = string row.["name"]
            physicalDefense = float row.["physicalDefense"]
        })
    // |> Set.map (fun (defenseClass: PhysicalDefense) -> defenseClass.name, defenseClass)
    // |> Map.ofSeq

    // SkillDiceModEffect

    let skillDiceModEffectSet =
        makeFogentRoleplayDataSet "SkillDiceModEffect.csv" (fun row -> {
            name = string row.["Name"]
            skillToEffect = string row.["Skill"]
            diceMod = parseDicePoolModString row.["Dice Mod"]
        })
    // let skillDiceModEffectSet = skillDiceModEffectSet.Values |> Set.ofSeq
    // // AttributeStatAdjustmentEffect
    let attributeStatAdjustmentEffectData =
        makeFogentRoleplayDataSet "AttributeStatAdjustmentEffect.csv" (fun row -> {
            name = string row.["Name"]
            attribute = AttributeName row.["Attribute"]
            adjustment = int row.["Adjustment"]
        })

    // let attributeStatAdjustmentEffectMap =
    //     attributeStatAdjustmentEffectData
    //     |> List.map (fun (attributeStatAdjustmentEffect: AttributeStatAdjustmentEffect) ->
    //         attributeStatAdjustmentEffect.name, attributeStatAdjustmentEffect)
    //     |> Map.ofList

    // AttributeDeterminedDiceModEffect

    let attributeDeterminedDiceModSet =
        makeFogentRoleplayDataSet "AttributeDeterminedDiceModEffectData.csv" (fun row -> {
            name = row.["name"]
            attributesToEffect = stringToAttributes row.["attributesToEffect"]
            dicePoolMod = parseDicePoolModString row.["dicePoolMod"]
        })

    let attributeDeterminedDiceModMap =
        attributeDeterminedDiceModSet
        |> Set.map (fun (attributeDeterminedDiceModEffect: AttributeDeterminedDiceMod) ->
            attributeDeterminedDiceModEffect.name, attributeDeterminedDiceModEffect)
        |> Map.ofSeq

    // WeightClass
    let weightClassData: WeightClass Set =
        makeFogentRoleplayDataSet "WeightClassData.csv" (fun row -> {
            name = row.["name"]
            bottomPercent = float row.["bottomPercent"]
            topPercent = float row.["topPercent"]
            attributeDeterminedDiceModEffect =
                attributeDeterminedDiceModMap.Item row.["attributeDeterminedDiceModEffect"]
        })

    // // MovementSpeedCalculation
    // let movementSpeedCalculationMap =
    //     makeFogentRoleplayDataSet "MovementSpeedCalculationData.csv" (fun row -> {
    //         name = string row.["desc"]
    //         baseMovementSpeed = uint row.["baseMovementSpeed"]
    //         governingAttribute = attributeMap.Item row.["governingAttributes"]
    //         feetPerAttributeLvl = uint row.["feetPerAttributeLvl"]
    //         governingSkill = string row.["governingSkill"]
    //         feetPerSkillLvl = uint row.["feetPerSkillLvl"]
    //     })
    //     |> Set.map (fun movementSpeedCalculationData -> movementSpeedCalculationData.name, movementSpeedCalculationData)
    //     |> Map.ofSeq

    // // CarryWeightCalculationu
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

    // Effect
    let effectDataMap =
        [
            Set.map Weapon weaponSet
            // Set.map Conduit conduitSet
            Set.map WeaponResource weaponResourceSet
            Set.map Container containerSet
            Set.map SkillDiceMod skillDiceModEffectSet
            //Set.map AttributeStatAdjustment attributeStatAdjustmentEffectData
            Set.map PhysicalDefense physicalDefenseSet
            Set.map AttributeDeterminedDiceMod attributeDeterminedDiceModSet
        ]
        |> Set.unionMany
        |> Set.map (fun (effect: Effect) -> effectToEffectName effect, effect)
        |> Map.ofSeq

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



    // Item
    let stringToEffectSet (effectMap: Map<string, Effect>) (input: string) =
        input.Split ", "
        |> Set.ofArray
        |> Set.filter (fun effectName -> effectMap.Keys.Contains effectName)
        |> Set.map (fun effectName -> effectDataMap.Item effectName)

    let itemStackMap =
        makeFogentRoleplayDataSet "ItemData.csv" (fun row -> {
            quantity = uint row.["quantity"]
            item = {
                name = string row.["desc"]
                itemEffectSet = stringToEffectSet effectDataMap row.["itemClasses"]
                itemTier = itemTierMap.Item row.["itemTier"]
                value = string row.["value"]
                weight = float row.["weight"]
            }
        })
        |> Set.map (fun itemStack -> (itemStack.item.name, itemStack))
        |> Map.ofSeq


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
                defaultAttributeSet = FogentRoleplayServerData.attributeNameSet
                itemStackMap = FogentRoleplayServerData.itemStackMap
                weaponSkillNameSet = FogentRoleplayServerData.weaponGoverningSkillNameSet
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