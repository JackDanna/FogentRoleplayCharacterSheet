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
    open FogentRoleplayLib.MagicSkill
    open FogentRoleplayLib.Weapon
    open FogentRoleplayLib.DicePoolMod
    open FogentRoleplayLib.TypeUtils
    open FogentRoleplayLib.AttributeName
    open FogentRoleplayLib.Container
    open FogentRoleplayLib.WeaponResource
    open FogentRoleplayLib.ItemTier
    open FogentRoleplayLib.PhysicalDefense
    open FogentRoleplayLib.SkillDiceMod
    open FogentRoleplayLib.SetAreaOfEffect
    open FogentRoleplayLib.AreaOfEffect
    open FogentRoleplayLib.AreaOfEffectCalculation
    open FogentRoleplayLib.AttributeDeterminedDiceMod
    open FogentRoleplayLib.WeightClass
    //open FogentRoleplayLib.MovementSpeedEffect
    open FogentRoleplayLib.Effect
    open FogentRoleplayLib.AttributeStatAdjustment
    open FogentRoleplayLib.ItemStack
    open FogentRoleplayLib.WeaponSpell
    open FogentRoleplayLib.MagicSystem
    open FogentRoleplayLib.AttributeName
    open FogentRoleplayLib.SkillName
    open FogentRoleplayLib.AttributeAndCoreSkillsData
    open FogentRoleplayLib.WeaponSkillData
    open FogentRoleplayLib.ParsingUtils
    open FogentRoleplayLib.TextEffect

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

    let parseMaxRangeOption input =
        match input with
        | "" -> None
        | validInput -> Some(uint validInput)

    let rangeMap =
        (makeFogentRoleplayDataList "CalculatedRangeData.csv" (fun row -> {
            name = string row.["name"]
            effectiveRange = uint row.["effectiveRange"]
            maxRangeOption = parseMaxRangeOption row.["maxRangeOption"]
         }),
         makeFogentRoleplayDataList "RangeCalculationData.csv" (fun row -> {
             name = string row.["name"]
             numDicePerEffectiveRangeUnit = uint row.["numDicePerEffectiveRangeUnit"]
             ftPerEffectiveRangeUnit = uint row.["ftPerEffectiveRangeUnit"]
             roundEffectiveRangeUp = Bool row.["roundEffectiveRangeUp"]
             maxRangeOption = parseMaxRangeOption row.["maxRangeOption"]
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

    let attributeAndCoreSkillDataSet, attributeNameSet =
        let attributeAndCoreSkillTupleSet =
            makeFogentRoleplayDataSet "CoreSkillData.csv" (fun row ->
                (SkillName row.["name"], AttributeName row.["governingAttribute"]))

        let attributeNameSet: AttributeName Set =
            attributeAndCoreSkillTupleSet
            |> Set.map (fun (_, governingAttributeName) -> governingAttributeName)
            |> Set.singleton
            |> Set.unionMany

        attributeNameSet
        |> Set.map (fun governingAttributeName -> {
            governingAttributeName = governingAttributeName
            coreSkillNameSet =
                attributeAndCoreSkillTupleSet
                |> Set.filter (fun (_, tempGoveringAttributeName) ->
                    tempGoveringAttributeName = governingAttributeName)
                |> Set.map (fun (skillName, _) -> skillName)
        }),
        attributeNameSet

    let attributeNameMap = stringSetToTypeMap attributeNameSet

    let stringToAttributes = mapAndStringToValueSet attributeNameMap

    //MagicSkillData
    let magicSkillDataMap =
        makeFogentRoleplayDataSet "MagicSkillData.csv" (fun row ->
            (string row.["name"],
             {
                 name = string row.["name"]
                 damageTypes = stringToDamageTypeSet (string row.["damageTypes"])
                 isMeleeCapable = Bool row.["meleeCapable"]
                 isRangeCapable = Bool row.["rangeCapable"]
             }))
        |> Map.ofSeq

    // MagicSystem
    let magicSystemData =
        makeFogentRoleplayDataSet "MagicSystemData.csv" (fun row -> {
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
        |> Set.map (fun magicSystem -> magicSystem.name, magicSystem)
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
        })

    let weaponSkillDataSet =
        makeFogentRoleplayDataSet "WeaponSkillData.csv" (fun row -> {
            name = string row.["skillName"]
            governingAttributes = stringToAttributes row.["governingAttributes"]
            governedWeapons = commaSeperatedStringToSet row.["weaponsGoverned"]
        })

    // WeaponSpell
    let weaponSpellSet: WeaponSpell Set =
        makeFogentRoleplayDataSet "WeaponSpellData.csv" (fun row -> {
            name = string row.["name"]
            oneHandedWeaponDice = parseDicePoolModOptionString row.["oneHandedWeaponDice"]
            twoHandedWeaponDice = parseDicePoolModOptionString row.["twoHandedWeaponDice"]
            dualWieldableBonus = parseDicePoolModOptionString row.["dualWieldableBonus"]
            penetration = uint row.["penetration"]
            range = rangeMap.Item row.["range"]
            engageableOpponents = engageableOpponentsMap row.["engageableOpponents"]
            areaOfEffectOption = namedAreaOfEffectOptionMap row.["areaOfEffect"]
            magicResourceAmount = uint row.["magicResourceAmount"]
        })

    // Container
    let containerSet =
        makeFogentRoleplayDataList "ContainerClassData.csv" (fun row -> {
            name = string row.["Name"]
            weightCapacity = float row.["Weight Capacity"]
            volumeFtCubed = float row.["Volume"]
        })
        |> Set.ofList

    let containerClassMap =
        Set.map (fun (containerClass: Container) -> containerClass.name, containerClass) containerSet
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
            durationAndSource = {
                duration = row.["duration"]
                source = row.["source"]
            }
        })
    // |> Set.map (fun (defenseClass: PhysicalDefense) -> defenseClass.name, defenseClass)
    // |> Map.ofSeq

    // SkillDiceModEffect

    let skillDiceModEffectSet =
        makeFogentRoleplayDataSet "SkillDiceModEffect.csv" (fun row -> {
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
        makeFogentRoleplayDataSet "Effect/AttributeStatAdjustment.csv" (fun row -> {
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
        makeFogentRoleplayDataSet "Effect/AttributeDeterminedDiceMod.csv" (fun row -> {
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

    // TextEffectForDisplay
    let textEffect: TextEffect Set =
        makeFogentRoleplayDataSet "Effect/TextEffect.csv" (fun row -> {
            name = string row.["Name"]
            effect = string row.["Effect"]
            durationAndSource = {
                duration = row.["duration"]
                source = row.["source"]
            }
        })

    // Effect
    let effectDataMap =
        [
            Set.map Weapon weaponSet
            Set.map WeaponResource weaponResourceSet
            Set.map Container containerSet
            Set.map SkillDiceMod skillDiceModEffectSet
            Set.map AttributeStatAdjustment attributeStatAdjustmentEffectData
            Set.map PhysicalDefense physicalDefenseSet
            Set.map AttributeDeterminedDiceMod attributeDeterminedDiceModSet
            Set.map TextEffect textEffect
        ]
        |> Set.unionMany
        |> Set.map (fun (effect: Effect) -> effectToEffectName effect, effect)
        |> Map.ofSeq

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
        input
        |> commaSeperatedStringToSet
        |> Set.filter (fun effectName -> effectMap.Keys.Contains effectName)
        |> Set.map (fun effectName -> effectMap.Item effectName)

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

let fallenDataApi: IFogentRoleplayDataApi = {
    getInitData =
        fun () -> async {
            return {
                attributeNameSet = FogentRoleplayServerData.attributeNameSet
                attributeAndCoreSkillDataSet = FogentRoleplayServerData.attributeAndCoreSkillDataSet
                itemStackMap = FogentRoleplayServerData.itemStackMap
                weaponSpellSet = FogentRoleplayServerData.weaponSpellSet
                magicSystemMap = FogentRoleplayServerData.magicSystemData
                weaponSkillData = FogentRoleplayServerData.weaponSkillDataSet
                effectMap = FogentRoleplayServerData.effectDataMap
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