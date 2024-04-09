module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

module FogentRoleplayServerData =
    open FSharp.Data
    open FogentRoleplayLib.TypeUtils
    open FogentRoleplayLib.Attribute
    open FogentRoleplayLib.Neg1To5
    open FogentRoleplayLib.DicePool
    // open FogentRoleplayLib.DamageType
    // open FogentRoleplayLib.TypeUtils
    // open FogentRoleplayLib.EngageableOpponents
    // open FogentRoleplayLib.Range
    // open FogentRoleplayLib.ResourceClass
    // open FogentRoleplayLib.Attribute
    // open FogentRoleplayLib.MagicSkill
    // open FogentRoleplayLib.AreaOfEffect
    // open FogentRoleplayLib.Neg1To4
    // open FogentRoleplayLib.Dice
    // open FogentRoleplayLib.MagicCombat
    // open FogentRoleplayLib.WeaponClass
    // open FogentRoleplayLib.ConduitClass
    // open FogentRoleplayLib.PhysicalDefenseEffect
    // open FogentRoleplayLib.WeaponResourceClass
    // open FogentRoleplayLib.ItemTier
    // open FogentRoleplayLib.Item
    // open FogentRoleplayLib.MovementSpeedEffect
    // open FogentRoleplayLib.SkillDiceModEffect
    // open FogentRoleplayLib.SkillDiceModEffectForDisplay
    // open FogentRoleplayLib.AttributeStatAdjustmentEffect
    // open FogentRoleplayLib.Effect
    // open FogentRoleplayLib.ContainerClass
    // open FogentRoleplayLib.TextEffectForDisplay
    // open FogentRoleplayLib.EffectForDisplay
    // open FogentRoleplayLib.CarryWeightCalculation
    // open FogentRoleplayLib.AttributeDeterminedDiceModEffect
    // open FogentRoleplayLib.AttributeDeterminedDiceModEffectForDisplay
    // open FogentRoleplayLib.WeightClass
    open FogentRoleplayLib.CoreSkill
    open FogentRoleplayLib.Skill
    // open FogentRoleplayLib.ItemStack

    let makeFogentRoleplayDataPath fileName =
        __SOURCE_DIRECTORY__
        + "../../../FogentRoleplayData/"
        + fileName

    let makeFallenData fileName mappingFunc =
        CsvFile
            .Load(
                makeFogentRoleplayDataPath fileName,
                hasHeaders = true
            )
            .Rows
        |> Seq.map (mappingFunc)
        |> List.ofSeq

    let Bool boolString =
        match boolString with
        | "TRUE" -> true
        | "FALSE" -> false
        | _ -> failwith ("Error: returns " + boolString)

    // // DamageType
    // let damageTypeData =
    //     makeFallenData "DamageTypeData.csv" (fun row -> (DamageType row.["desc"]))

    // let stringToDamageTypeList =
    //     damageTypeData
    //     |> stringListToTypeMap
    //     |> stringAndMapToDamageTypeList

    // // EngageableOpponents
    // let engageableOpponentsCalculationData =
    //     makeFallenData "EngageableOpponentsCalculationData.csv" (fun row ->
    //         { name = string row.["desc"]
    //           combatRollDivisor = uint row.["combatRollDivisor"]
    //           maxEO = parseMaxEngageableOpponentsString row.["maxEO"] })

    // let engageableOpponentsMap =
    //     parseEngaeableOpponentsString (eoCalculationListToMap engageableOpponentsCalculationData)

    // // Range
    // let calculatedRangeData =
    //     makeFallenData "CalculatedRangeData.csv" (fun row ->
    //         { name = string row.["desc"]
    //           effectiveRange = uint row.["effectiveRange"]
    //           maxRange = uint row.["maxRange"] })

    // let rangeCalculationData =
    //     makeFallenData "RangeCalculationData.csv" (fun row ->
    //         { name = string row.["desc"]
    //           numDicePerEffectiveRangeUnit = uint row.["numDicePerEffectiveRangeUnit"]
    //           ftPerEffectiveRangeUnit = uint row.["ftPerEffectiveRangeUnit"]
    //           roundEffectiveRangeUp = Bool row.["roundEffectiveRangeUp"]
    //           maxRange = uint row.["maxRange"] })

    // let rangeMap = createRangeMap calculatedRangeData rangeCalculationData

    // let rangeOptionMap string =
    //     match string with
    //     | "None" -> None
    //     | _ -> rangeMap.Item string |> Some

    // // ResourceClass
    // let resourceClassData =
    //     makeFallenData "ResourceClassData.csv" (fun row -> (ResourceClass row.["desc"]))

    // let resourceClassMap = stringListToTypeMap resourceClassData

    // let resourceClassOptionMap string =
    //     match string with
    //     | "None" -> None
    //     | _ -> Some <| resourceClassMap.Item string

    // AttributeAndCoreSkill
    let attributeData: Attribute list =
        makeFallenData 
            "AttributeData.csv" 
            (fun row -> Attribute row.["desc"] )

    let coreSkillData: CoreSkill list =
        makeFallenData 
            "CoreSkillData.csv" 
            (fun row ->
                { skill = { name = row.["name"]; level = Zero; dicePool = baseDicePool }
                  governingAttribute = row.["governingAttribute"]}
            )

    let attributeMap = stringListToTypeMap attributeData

    let mapAndStringToAttributes (attributeMap: Map<string, Attribute>) (input) =
        String.filter ((<>) ' ') input
        |> (fun s -> s.Split(',', System.StringSplitOptions.RemoveEmptyEntries))
        |> List.ofArray
        |> List.map (fun attributeString -> attributeMap.Item attributeString)

    let stringToAttributes = mapAndStringToAttributes attributeMap

    // MagicSkill
    // let magicSkillData =
    //     makeFallenData "MagicSkillData.csv" (fun row ->
    //         { name = string row.["desc"]
    //           damageTypes = stringToDamageTypeList (string row.["damageTypes"])
    //           rangeAdjustment = int row.["rangeAdjustment"]
    //           isMeleeCapable = Bool row.["meleeCapable"]
    //           resourceClass = string row.["magicResourceClass"] })

    // let magicSkillMap =
    //     List.map (fun (magicSkill: MagicSkill) -> magicSkill.name, magicSkill) magicSkillData
    //     |> Map.ofList

    // // MagicCombat
    // let magicCombatData =
    //     makeFallenData "MagicCombatData.csv" (fun row ->
    //         { name = string row.["Description"]
    //           lvlRequirment = int row.["Lvl Requirment"] |> intToNeg1To4
    //           dicePoolMod = parseDicePoolModString row.["Dice Mod"]
    //           penetration = uint row.["Penetration"]
    //           range = rangeMap.Item(string row.["Range"])
    //           engageableOpponents = engageableOpponentsMap row.["Engageable Opponents"]
    //           minResourceRequirment = uint row.["Resource Requirment"]
    //           areaOfEffect = AreaOfEffectOptionMap.Item row.["Area Of Effect"] })

    // let magicCombatMap =
    //     List.map (fun (magicCombat: MagicCombat) -> magicCombat.name, magicCombat) magicCombatData
    //     |> Map.ofList

    // // WeaponClass
    // let weaponClassData =
    //     makeFallenData "WeaponClassData.csv" (fun row ->
    //         { name = string row.["desc"]
    //           oneHandedWeaponDice = parseDicePoolModOptionString row.["oneHandedWeaponDice"]
    //           twoHandedWeaponDice = parseDicePoolModString row.["twoHandedWeaponDice"]
    //           penetration = uint row.["penetration"]
    //           range = rangeMap.Item row.["range"]
    //           damageTypes = stringToDamageTypeList row.["damageTypes"]
    //           engageableOpponents = engageableOpponentsMap row.["engageableOpponents"]
    //           dualWieldableBonus = parseDicePoolModOptionString row.["dualWieldableBonus"]
    //           areaOfEffect = AreaOfEffectOptionMap.Item row.["areaOfEffect"]
    //           resourceClass = resourceClassOptionMap row.["resourceClass"] })

    // let weaponClassMap =
    //     List.map (fun (weaponClass: WeaponClass) -> weaponClass.name, weaponClass) weaponClassData
    //     |> Map.ofList

    // // ConduitClass
    // let conduitClassData =
    //     makeFallenData "ConduitClassData.csv" (fun row ->

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

    // // ContainerClass
    // let containerClassData =
    //     makeFallenData "ContainerClassData.csv" (fun row ->
    //         { name = string row.["Name"]
    //           weightCapacity = float row.["Weight Capacity"] })

    // let containerClassMap =
    //     List.map (fun (containerClass: ContainerClass) -> containerClass.name, containerClass) containerClassData
    //     |> Map.ofList

    // // DefenseClass
    // let physicalDefenseEffectData: PhysicalDefenseEffect list =
    //     makeFallenData "PhysicalDefenseEffect.csv" (fun row ->
    //         { name = string row.["desc"]
    //           physicalDefense = float row.["physicalDefense"] })

    // let physicalDefenseEffectMap =
    //     physicalDefenseEffectData
    //     |> List.map (fun (defenseClass: PhysicalDefenseEffect) -> defenseClass.name, defenseClass)
    //     |> Map.ofList

    // // SkillDiceModEffect
    // let skillDiceModEffectData: SkillDiceModEffect list =
    //     makeFallenData "SkillDiceModEffect.csv" (fun row ->
    //         { name = string row.["Name"]
    //           skillToEffect = string row.["Skill"]
    //           diceMod = parseDicePoolModString row.["Dice Mod"] })

    // let skillDiceModEffectMap =
    //     skillDiceModEffectData
    //     |> List.map (fun (skillAdjustment: SkillDiceModEffect) -> skillAdjustment.name, skillAdjustment)
    //     |> Map.ofList

    // // AttributeStatAdjustmentEffect
    // let attributeStatAdjustmentEffectData =
    //     makeFallenData "AttributeStatAdjustmentEffect.csv" (fun row ->
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
    //     makeFallenData "AttributeDeterminedDiceModEffectData.csv" (fun row ->
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
    //     makeFallenData "WeightClassData.csv" (fun row ->
    //         { name = row.["name"]
    //           bottomPercent = float row.["bottomPercent"]
    //           topPercent = float row.["topPercent"]
    //           attributeDeterminedDiceModEffect =
    //             attributeDeterminedDiceModEffectMap.Item row.["attributeDeterminedDiceModEffect"] })

    // // MovementSpeedCalculation
    // let movementSpeedCalculationData =
    //     makeFallenData "MovementSpeedCalculationData.csv" (fun row ->
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
    //     makeFallenData "CarryWeightCalculationData.csv" (fun row ->
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
    //     makeFallenData "CharacterEffectForDisplayData.csv" (fun row ->
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

    // // WeaponResourceClass
    // let weaponResourceClassData =
    //     makeFallenData "WeaponResourceClassData.csv" (fun row ->
    //         { name = string row.["desc"]
    //           resourceClass = resourceClassMap.Item row.["resourceClass"]
    //           resourceDice = parseDicePoolModString row.["resourceDice"]
    //           penetration = uint row.["penetration"]
    //           range = rangeOptionMap row.["range"]
    //           damageTypes = stringToDamageTypeList row.["damageTypes"]
    //           areaOfEffect = AreaOfEffectOptionMap.Item row.["areaOfEffect"] })

    // let weaponResourceClassMap =
    //     List.map
    //         (fun (weaponResourceClass: WeaponResourceClass) -> weaponResourceClass.name, weaponResourceClass)
    //         weaponResourceClassData
    //     |> Map.ofList

    // // ItemTier
    // let itemTierData =
    //     makeFallenData "ItemTierData.csv" (fun row ->
    //         { name = string row.["desc"]
    //           level = int row.["level"]
    //           runeSlots = uint row.["runeSlots"]
    //           baseDice = parseDicePoolString row.["baseDice"]
    //           durabilityMax = uint row.["durabilityMax"] })

    // let itemTierMap =
    //     List.map (fun (itemTier: ItemTier) -> itemTier.name, itemTier) itemTierData
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
    //     makeFallenData "ItemData.csv" (fun row ->
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

let fallenDataApi: IFogentRoleplayDataApi =
    { getInitData =
        fun () ->
            async {
                return
                    { defaultCoreSkillList = FogentRoleplayServerData.coreSkillData
                      defaultAttributeList = FogentRoleplayServerData.attributeData
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
            } }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue fallenDataApi
    |> Remoting.buildHttpHandler

let app =
    application {
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

[<EntryPoint>]
let main _ =
    run app
    0