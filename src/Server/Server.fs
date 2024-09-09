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
    open FogentRoleplayLib.Weapon
    open FogentRoleplayLib.DicePoolMod
    open FogentRoleplayLib.TypeUtils
    open FogentRoleplayLib.AttributeName
    open FogentRoleplayLib.Container
    open FogentRoleplayLib.WeaponResource
    open FogentRoleplayLib.PhysicalDefense
    open FogentRoleplayLib.SkillDiceMod
    open FogentRoleplayLib.SetAreaOfEffect
    open FogentRoleplayLib.AreaOfEffect
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

    let damageTypes =
        makeFogentRoleplayDataSet "DamageTypeData.csv" (fun row -> (DamageType row.["name"]))

    let stringToDamageTypeSet =
        damageTypes |> stringSetToTypeMap |> mapAndStringToValueSet

    // EngageableOpponents

    let engageableOpponentsCalculations: Set<EngageableOpponentsCalculation> =
        makeFogentRoleplayDataSet "EngageableOpponentsCalculationData.csv" (fun row -> {
            name = string row.["name"]
            combatRollDivisor = uint row.["combatRollDivisor"]
            maxEOOption = parseMaxEngageableOpponentsString row.["maxEO"]
        })

    let engageableOpponentsMap =
        engageableOpponentsCalculations
        |> eoCalculationSetToMap
        |> parseEngaeableOpponentsString

    // Range
    let parseMaxRangeOption input =
        match input with
        | "" -> None
        | validInput -> Some(uint validInput)

    let calculatedRanges: CalculatedRange list =
        makeFogentRoleplayDataList "CalculatedRangeData.csv" (fun row -> {
            name = string row.["name"]
            effectiveRange = uint row.["effectiveRange"]
            maxRangeOption = parseMaxRangeOption row.["maxRangeOption"]
        })

    let rangeCalculations =
        makeFogentRoleplayDataList "RangeCalculationData.csv" (fun row -> {
            name = string row.["name"]
            numDicePerEffectiveRangeUnit = uint row.["numDicePerEffectiveRangeUnit"]
            ftPerEffectiveRangeUnit = uint row.["ftPerEffectiveRangeUnit"]
            roundEffectiveRangeUp = Bool row.["roundEffectiveRangeUp"]
            maxRangeOption = parseMaxRangeOption row.["maxRangeOption"]
        })

    let rangeMap = (calculatedRanges, rangeCalculations) ||> createRangeMap

    let rangeOptionMap string =
        match string with
        | "None" -> None
        | _ -> rangeMap.Item string |> Some

    // AreaOfEffect

    let sphereCalculationSet =
        makeFogentRoleplayDataSet "AreaOfEffects/SphereCalculation.csv" (fun row -> {
            name = string row.["name"]
            initRadius = float row.["Init Radius"]
            radiusPerDice = float row.["Radius per Dice"]
        })

    let coneCalculationSet =
        makeFogentRoleplayDataSet "AreaOfEffects/ConeCalculation.csv" (fun row -> {
            name = string row.["name"]
            angle = float row.["angle"]
            initBaseAndHeight = float row.["init triangle base/height"]
            baseAndHeightPerDice = float row.["base/height per unit"]
        })

    let setSphereSet =
        makeFogentRoleplayDataSet "AreaOfEffects/SetSphere.csv" (fun row -> {
            name = row.["Name"]
            radius = uint row.["Radius(ft)"]
        })

    let setConeSet =
        makeFogentRoleplayDataSet "AreaOfEffects/SetCone.csv" (fun row -> {
            name = string row.["name"]
            baseAndHeight = uint row.["Triangle Base/Height (ft)"]
            angle = float row.["Cone Angle (degrees)"]
        })

    let areaOfEffectMap =
        [
            Set.map (fun (setCone: SetCone) -> (setCone.name, setCone |> SetCone |> SetAreaOfEffect)) setConeSet
            Set.map
                (fun (setSphere: SetSphere) -> (setSphere.name, setSphere |> SetSphere |> SetAreaOfEffect))
                setSphereSet
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

    let coreSkillDataSet: Set<CoreSkillData> =
        makeFogentRoleplayDataSet "CoreSkillData.csv" (fun row -> {
            skillName = SkillName row.["name"]
            attributeName = AttributeName row.["governingAttribute"]
        })

    let attributeNameSet =
        coreSkillDataSet |> Set.map (fun coreSkillData -> coreSkillData.attributeName)

    let attributeNameMap = stringSetToTypeMap attributeNameSet

    let stringToAttributes = mapAndStringToValueSet attributeNameMap

    //MagicSkillData
    let magicSkillDataSet: Set<MagicSkillData> =
        makeFogentRoleplayDataSet "MagicSkillData.csv" (fun row -> {
            name = SkillName row.["name"]
            damageTypes = stringToDamageTypeSet (string row.["damageTypes"])
            isMeleeCapable = Bool row.["meleeCapable"]
            isRangeCapable = Bool row.["rangeCapable"]
        })

    let magicSkillDataMap =
        magicSkillDataSet
        |> Set.map (fun magicSkill -> magicSkill.name, magicSkill)
        |> Map.ofSeq

    // MagicSystem
    let magicSystemData =
        makeFogentRoleplayDataSet "MagicSystemData.csv" (fun row -> {
            name = row.["name"]
            vocationName = row.["vocationName"]
            vocationGoverningAttributeSet = stringToAttributes row.["vocationGoverningAttributeSet"]
            resourceName = row.["resourceName"]
            governingCoreSkill = row.["governingCoreSkill"]
            magicSkillDataMap =
                row.["magicSkillNameSet"]
                |> commaSeperatedStringToSet
                |> Set.map (fun key -> magicSkillDataMap.Item key)
                |> Set.map (fun magicSkillData -> magicSkillData.name, magicSkillData)
                |> Map.ofSeq
        })
        |> Set.map (fun magicSystem -> magicSystem.name, magicSystem)
        |> Map.ofSeq

    // WeaponClass
    let weaponSet =
        makeFogentRoleplayDataSet "WeaponClassData.csv" (fun row -> {
            name = string row.["name"]
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
        })

    // WeaponSkillData
    let weaponSkillDataMap =
        makeFogentRoleplayDataSet "WeaponSkillData.csv" (fun row -> {
            name = string row.["skillName"]
            governingAttributes = stringToAttributes row.["governingAttributes"]
        })
        |> Set.map (fun weaponSkillData -> weaponSkillData.name, weaponSkillData)
        |> Map.ofSeq

    // WeaponSpell
    let weaponSpellSet: WeaponSpell Set =
        makeFogentRoleplayDataSet "WeaponSpellData.csv" (fun row -> {
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
        makeFogentRoleplayDataSet "ContainerClassData.csv" (fun row -> {
            name = string row.["Name"]
            weightCapacity = float row.["Weight Capacity"]
            volumeFtCubed = float row.["Volume"]
        })
        |> Set.map (fun containerMap -> containerMap.name, containerMap)
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

    // PhysicalDefense
    let physicalDefenseSet =
        makeFogentRoleplayDataSet "PhysicalDefenseEffect.csv" (fun row -> {
            name = string row.["name"]
            physicalDefense = float row.["physicalDefense"]
            durationAndSource = {
                duration = row.["duration"]
                source = row.["source"]
            }
        })

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

    // BaseDiceTiers
    open FogentRoleplayLib.BaseDiceTier

    let baseDiceTiers =
        makeFogentRoleplayDataSet "BaseDiceTierData.csv" (fun row -> {
            itemPrefix = string row.["itemPrefix"]
            level = int row.["level"]
            baseDice = parseDicePoolString row.["baseDice"]
        //itemDurabilityMax = uint row.["itemDurabilityMax"]
        })

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
        makeFogentRoleplayDataSet "WeightClassData.csv" (fun row -> {
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
            attributeDeterminedDiceModEffect =
                attributeDeterminedDiceModMap.Item row.["attributeDeterminedDiceModEffect"]
        })

    // Combat Speed
    open FogentRoleplayLib.SpeedCalculation
    open FogentRoleplayLib.CombatSpeedCalculation

    let speedMap: Map<string, SpeedCalculation> =
        makeFogentRoleplayDataSet "Speed.csv" (fun row -> {
            name = row.["name"]
            feetPerGoverningSkillDice = float row.["feetPerGoverningSkillDice"]
            feetPerReactionSpeedAttribute = float row.["feetPerReactionSpeedAttribute"]
        })
        |> Set.map (fun speed -> (speed.name, speed))
        |> Map.ofSeq

    let combatSpeedCalculationMap =
        makeFogentRoleplayDataSet "CombatSpeed.csv" (fun row -> {
            name = row.["name"]
            governingSkillName = SkillName row.["governingSkillName"]
            reactionSpeedAttributeName = AttributeName row.["reactionSpeedAttributeName"]
            speed = speedMap.Item row.["speed"]
        })
        |> Set.map (fun combatSpeed -> combatSpeed.name, combatSpeed)
        |> Map.ofSeq

    // CarryWeightCalculation
    open FogentRoleplayLib.CarryWeightCalculation

    let carryWeightCalculationMap =
        makeFogentRoleplayDataSet "CarryWeightCalculationData.csv" (fun row -> {
            name = string row.["name"]
            baseWeight = uint row.["baseWeight"]
            governingAttribute = AttributeName row.["governingAttribute"]
            weightIncreasePerAttribute = uint row.["weightIncreasePerAttribute"]
            governingSkill = string row.["governingSkill"]
            weightIncreasePerSkill = uint row.["weightIncreasePerSkill"]
        })
        |> Set.map (fun carryWeightCalculation -> carryWeightCalculation.name, carryWeightCalculation)
        |> Map.ofSeq

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
            Set.map SkillDiceMod skillDiceModEffectSet
            Set.map AttributeStatAdjustment attributeStatAdjustmentEffectData
            Set.map PhysicalDefense physicalDefenseSet
            Set.map AttributeDeterminedDiceMod attributeDeterminedDiceModSet
            Set.map BaseDiceMod baseDiceModSet
            Set.map TextEffect textEffect
        ]
        |> Set.unionMany
        |> Set.map (fun (effect: Effect) -> effectToEffectName effect, effect)
        |> Map.ofSeq

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

    let itemStackMap =
        makeFogentRoleplayDataSet "ItemData.csv" (fun row ->

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
        |> Set.toList
        |> List.choose id
        |> List.map (fun itemElement -> (itemElementToName itemElement, itemElement))
        |> Map.ofSeq

open Npgsql
open Npgsql.FSharp

module Database =

    let databaseConnectionString =
        let host = System.Environment.GetEnvironmentVariable "PGHOST"
        let username = System.Environment.GetEnvironmentVariable "PGUSER"
        let password = ""
        let database = System.Environment.GetEnvironmentVariable "PGDATABASE"

        let connectionString =
            $"Host={host};Username={username};Password={password};Database={database}"

        let builder = new NpgsqlConnectionStringBuilder(connectionString)
        builder.ToString()

    let databaseConnection = databaseConnectionString |> Sql.connect

    module DamageTypesDatabase =
        open FogentRoleplayLib.DamageType

        let damageTypesTableName = "damage_types"
        let nameHeader = "name"

        let initDamageTypesTable () =
            databaseConnection
            |> Sql.query
                $"""
                CREATE TABLE IF NOT EXISTS {damageTypesTableName} (
                    {nameHeader} VARCHAR(100) PRIMARY KEY
                )
            """
            |> Sql.executeNonQuery
            |> function
                | affectedRows ->
                    printfn "Table %s created successfully. Rows affected: %d" damageTypesTableName affectedRows

        let insertDamageType (damageType: DamageType) =
            databaseConnection
            |> Sql.query $"INSERT INTO {damageTypesTableName} ({nameHeader}) VALUES (@{nameHeader})"
            |> Sql.parameters [ $"@{nameHeader}", Sql.string damageType ]
            |> Sql.executeNonQuery

        let insertDamageTypes = Set.map insertDamageType

        let getDamageTypes () =
            databaseConnection
            |> Sql.query $"SELECT {nameHeader} FROM {damageTypesTableName}"
            |> Sql.execute (fun read -> read.string nameHeader |> DamageType)
            |> Set.ofList

    module EngageableOpponentsCalculationDatabase =
        open FogentRoleplayLib.EngageableOpponents // Assuming this is where EngageableOpponentsCalculation is defined

        let engageableOpponentsTableName = "engageable_opponents_calculation"
        let nameHeader = "name"
        let combatRollDivisorHeader = "combat_roll_divisor"
        let maxEOOptionHeader = "max_eo_option"

        let initEngageableOpponentsCalculationTable () =
            databaseConnection
            |> Sql.query
                $"""
                CREATE TABLE IF NOT EXISTS {engageableOpponentsTableName} (
                    {nameHeader} VARCHAR(100) PRIMARY KEY,
                    {combatRollDivisorHeader} INTEGER NOT NULL,
                    {maxEOOptionHeader} INTEGER
                )
            """
            |> Sql.executeNonQuery
            |> function
                | affectedRows ->
                    printfn "Table %s created successfully. Rows affected: %d" engageableOpponentsTableName affectedRows

        let insertEngageableOpponentsCalculation (calculation: EngageableOpponentsCalculation) =
            databaseConnection
            |> Sql.query
                $"INSERT INTO {engageableOpponentsTableName} ({nameHeader}, {combatRollDivisorHeader}, {maxEOOptionHeader}) VALUES (@{nameHeader}, @{combatRollDivisorHeader}, @{maxEOOptionHeader})"
            |> Sql.parameters [
                $"@{nameHeader}", Sql.string calculation.name
                $"@{combatRollDivisorHeader}", Sql.int (int calculation.combatRollDivisor)
                $"@{maxEOOptionHeader}", Sql.intOrNone (calculation.maxEOOption |> Option.map int)
            ]
            |> Sql.executeNonQuery

        let insertEngageableOpponentsCalculations =
            Set.map insertEngageableOpponentsCalculation

        let getEngageableOpponentsCalculations () =
            databaseConnection
            |> Sql.query
                $"SELECT {nameHeader}, {combatRollDivisorHeader}, {maxEOOptionHeader} FROM {engageableOpponentsTableName}"
            |> Sql.execute (fun read -> {
                name = read.string nameHeader
                combatRollDivisor = read.int combatRollDivisorHeader |> uint
                maxEOOption = read.intOrNone maxEOOptionHeader |> Option.map uint
            })
            |> Set.ofList

    module CalculatedRangesDatabase =
        open FogentRoleplayLib.Range
        let calculatedRangesTableName = "calculated_ranges"
        let nameHeader = "name"
        let effectRangeHeader = "effective_range"
        let maxRangeOptionHeader = "max_range_option"

        let initCalculatedRangesTable () =
            databaseConnection
            |> Sql.query
                $"""
                CREATE TABLE IF NOT EXISTS {calculatedRangesTableName} (
                    {nameHeader} VARCHAR(100) PRIMARY KEY,
                    {effectRangeHeader} INTEGER NOT NULL,
                    {maxRangeOptionHeader} INTEGER
                )
            """
            |> Sql.executeNonQuery
            |> function
                | affectedRows ->
                    printfn "Table %s created successfully. Rows affected: %d" calculatedRangesTableName affectedRows

        let insertCalculatedRange (range: CalculatedRange) =
            databaseConnection
            |> Sql.query
                $"INSERT INTO {calculatedRangesTableName} ({nameHeader}, {effectRangeHeader}, {maxRangeOptionHeader}) VALUES (@{nameHeader}, @{effectRangeHeader}, @{maxRangeOptionHeader})"
            |> Sql.parameters [
                $"@{nameHeader}", Sql.string range.name
                $"@{effectRangeHeader}", Sql.int (int range.effectiveRange)
                $"@{maxRangeOptionHeader}", Sql.intOrNone (range.maxRangeOption |> Option.map int)
            ]
            |> Sql.executeNonQuery

        let insertCalculatedRanges = List.map insertCalculatedRange

        let getCalculatedRanges () =
            databaseConnection
            |> Sql.query
                $"SELECT {nameHeader}, {effectRangeHeader}, {maxRangeOptionHeader} FROM {calculatedRangesTableName}"
            |> Sql.execute (fun read -> {
                name = read.string nameHeader
                effectiveRange = read.int effectRangeHeader |> uint
                maxRangeOption = read.intOrNone maxRangeOptionHeader |> Option.map uint
            })

    module RangeCalculationsDatabase =
        open FogentRoleplayLib.Range
        let rangeCalculationsTableName = "range_calculations"
        let nameHeader = "name"
        let numDicePerEffectiveRangeUnitHeader = "num_dice_per_effective_range_unit"
        let ftPerEffectiveRangeUnitHeader = "ft_per_effective_range_unit"
        let roundEffectiveRangeUpHeader = "round_effective_range_up"
        let maxRangeOptionHeader = "max_range_option"

        let initRangeCalculationTable () =
            databaseConnection
            |> Sql.query
                $"""
                CREATE TABLE IF NOT EXISTS {rangeCalculationsTableName} (
                    {nameHeader} VARCHAR(100) PRIMARY KEY,
                    {numDicePerEffectiveRangeUnitHeader} INTEGER NOT NULL,
                    {ftPerEffectiveRangeUnitHeader} INTEGER NOT NULL,
                    {roundEffectiveRangeUpHeader} BOOLEAN NOT NULL,
                    {maxRangeOptionHeader} INTEGER
                )
            """
            |> Sql.executeNonQuery
            |> function
                | affectedRows ->
                    printfn "Table %s created successfully. Rows affected: %d" rangeCalculationsTableName affectedRows

        let insertRangeCalculation (calc: RangeCalculation) =
            databaseConnection
            |> Sql.query
                $"""
                INSERT INTO {rangeCalculationsTableName}
                ({nameHeader}, {numDicePerEffectiveRangeUnitHeader}, {ftPerEffectiveRangeUnitHeader}, {roundEffectiveRangeUpHeader}, {maxRangeOptionHeader})
                VALUES (@{nameHeader}, @{numDicePerEffectiveRangeUnitHeader}, @{ftPerEffectiveRangeUnitHeader}, @{roundEffectiveRangeUpHeader}, @{maxRangeOptionHeader})
            """
            |> Sql.parameters [
                $"@{nameHeader}", Sql.string calc.name
                $"@{numDicePerEffectiveRangeUnitHeader}", Sql.int (int calc.numDicePerEffectiveRangeUnit)
                $"@{ftPerEffectiveRangeUnitHeader}", Sql.int (int calc.ftPerEffectiveRangeUnit)
                $"@{roundEffectiveRangeUpHeader}", Sql.bool calc.roundEffectiveRangeUp
                $"@{maxRangeOptionHeader}", Sql.intOrNone (calc.maxRangeOption |> Option.map int)
            ]
            |> Sql.executeNonQuery
            |> ignore

        let insertRangeCalculations = List.map insertRangeCalculation

        let getRangeCalculations () =
            databaseConnection
            |> Sql.query $"SELECT * FROM {rangeCalculationsTableName}"
            |> Sql.execute (fun read -> {
                name = read.string nameHeader
                numDicePerEffectiveRangeUnit = read.int numDicePerEffectiveRangeUnitHeader |> uint
                ftPerEffectiveRangeUnit = read.int ftPerEffectiveRangeUnitHeader |> uint
                roundEffectiveRangeUp = read.bool roundEffectiveRangeUpHeader
                maxRangeOption = read.intOrNone maxRangeOptionHeader |> Option.map uint
            })

    module SphereCalculationDatabase =
        open FogentRoleplayLib.AreaOfEffectCalculation

        let sphereCalculationsTableName = "sphere_calculations"
        let nameHeader = "name"
        let initRadiusHeader = "init_radius"
        let radiusPerDiceHeader = "radius_per_dice"

        let initSphereCalculationTable () =
            databaseConnection
            |> Sql.query
                $"""
                CREATE TABLE IF NOT EXISTS {sphereCalculationsTableName} (
                    {nameHeader} VARCHAR(100) PRIMARY KEY,
                    {initRadiusHeader} DECIMAL NOT NULL,
                    {radiusPerDiceHeader} DECIMAL NOT NULL
                )
            """
            |> Sql.executeNonQuery
            |> function
                | affectedRows ->
                    printfn "Table %s created successfully. Rows affected: %d" sphereCalculationsTableName affectedRows

        let insertSphereCalculation (calculation: SphereCalculation) =
            databaseConnection
            |> Sql.query
                $"INSERT INTO {sphereCalculationsTableName} ({nameHeader}, {initRadiusHeader}, {radiusPerDiceHeader}) VALUES (@{nameHeader}, @{initRadiusHeader}, @{radiusPerDiceHeader})"
            |> Sql.parameters [
                $"@{nameHeader}", Sql.string calculation.name
                $"@{initRadiusHeader}", Sql.decimal (decimal calculation.initRadius)
                $"@{radiusPerDiceHeader}", Sql.decimal (decimal calculation.radiusPerDice)
            ]
            |> Sql.executeNonQuery

        let insertSphereCalculations = Set.map insertSphereCalculation

        let getSphereCalculations () =
            databaseConnection
            |> Sql.query
                $"SELECT {nameHeader}, {initRadiusHeader}, {radiusPerDiceHeader} FROM {sphereCalculationsTableName}"
            |> Sql.execute (fun read -> {
                name = read.string nameHeader
                initRadius = read.float initRadiusHeader |> float
                radiusPerDice = read.float radiusPerDiceHeader |> float
            })
            |> Set.ofList

    module ConeCalculationDatabase =
        open FogentRoleplayLib.AreaOfEffectCalculation

        let coneCalculationsTableName = "cone_calculations"
        let nameHeader = "name"
        let initBaseAndHeightHeader = "init_base_and_height"
        let baseAndHeightPerDiceHeader = "base_and_height_per_dice"
        let angleHeader = "angle"

        let initConeCalculationTable () =
            databaseConnection
            |> Sql.query
                $"""
                CREATE TABLE IF NOT EXISTS {coneCalculationsTableName} (
                    {nameHeader} VARCHAR(100) PRIMARY KEY,
                    {initBaseAndHeightHeader} DECIMAL(10, 2) NOT NULL,
                    {baseAndHeightPerDiceHeader} DECIMAL(10, 2) NOT NULL,
                    {angleHeader} DECIMAL(10, 2) NOT NULL
                )
            """
            |> Sql.executeNonQuery
            |> function
                | affectedRows ->
                    printfn "Table %s created successfully. Rows affected: %d" coneCalculationsTableName affectedRows

        let insertConeCalculation (calculation: ConeCalculation) =
            databaseConnection
            |> Sql.query
                $"INSERT INTO {coneCalculationsTableName} ({nameHeader}, {initBaseAndHeightHeader}, {baseAndHeightPerDiceHeader}, {angleHeader}) VALUES (@{nameHeader}, @{initBaseAndHeightHeader}, @{baseAndHeightPerDiceHeader}, @{angleHeader})"
            |> Sql.parameters [
                $"@{nameHeader}", Sql.string calculation.name
                $"@{initBaseAndHeightHeader}", Sql.decimal (decimal calculation.initBaseAndHeight)
                $"@{baseAndHeightPerDiceHeader}", Sql.decimal (decimal calculation.baseAndHeightPerDice)
                $"@{angleHeader}", Sql.decimal (decimal calculation.angle)
            ]
            |> Sql.executeNonQuery

        let insertConeCalculations = Set.map insertConeCalculation

        let getConeCalculations () =
            databaseConnection
            |> Sql.query
                $"SELECT {nameHeader}, {initBaseAndHeightHeader}, {baseAndHeightPerDiceHeader}, {angleHeader} FROM {coneCalculationsTableName}"
            |> Sql.execute (fun read -> {
                name = read.string nameHeader
                initBaseAndHeight = read.decimal initBaseAndHeightHeader |> float
                baseAndHeightPerDice = read.decimal baseAndHeightPerDiceHeader |> float
                angle = read.decimal angleHeader |> float
            })
            |> Set.ofList

    module SetSphereDatabase =
        open FogentRoleplayLib.SetAreaOfEffect

        let setSpheresTableName = "set_spheres"
        let nameHeader = "name"
        let radiusHeader = "radius"

        let initSetSphereTable () =
            databaseConnection
            |> Sql.query
                $"""
                CREATE TABLE IF NOT EXISTS {setSpheresTableName} (
                    {nameHeader} VARCHAR(100) PRIMARY KEY,
                    {radiusHeader} INTEGER NOT NULL
                )
            """
            |> Sql.executeNonQuery
            |> function
                | affectedRows ->
                    printfn "Table %s created successfully. Rows affected: %d" setSpheresTableName affectedRows

        let insertSetSphere (setSphere: SetSphere) =
            databaseConnection
            |> Sql.query
                $"INSERT INTO {setSpheresTableName} ({nameHeader}, {radiusHeader}) VALUES (@{nameHeader}, @{radiusHeader})"
            |> Sql.parameters [
                $"@{nameHeader}", Sql.string setSphere.name
                $"@{radiusHeader}", Sql.int (int setSphere.radius)
            ]
            |> Sql.executeNonQuery

        let insertSetSpheres = Set.map insertSetSphere

        let getSetSpheres () : Set<SetSphere> =
            databaseConnection
            |> Sql.query $"SELECT {nameHeader}, {radiusHeader} FROM {setSpheresTableName}"
            |> Sql.execute (fun read -> {
                name = read.string nameHeader
                radius = read.int radiusHeader |> uint
            })
            |> Set.ofSeq

    module SetConeDatabase =
        open FogentRoleplayLib.SetAreaOfEffect // Assuming this is where SetCone is defined

        let setConesTableName = "set_cones"
        let nameHeader = "name"
        let baseAndHeightHeader = "base_and_height"
        let angleHeader = "angle"

        let initSetConeTable () =
            databaseConnection
            |> Sql.query
                $"""
                CREATE TABLE IF NOT EXISTS {setConesTableName} (
                    {nameHeader} VARCHAR(100) PRIMARY KEY,
                    {baseAndHeightHeader} INTEGER NOT NULL,
                    {angleHeader} DECIMAL(10, 2) NOT NULL
                )
            """
            |> Sql.executeNonQuery
            |> function
                | affectedRows ->
                    printfn "Table %s created successfully. Rows affected: %d" setConesTableName affectedRows

        let insertSetCone (setCone: SetCone) =
            databaseConnection
            |> Sql.query
                $"INSERT INTO {setConesTableName} ({nameHeader}, {baseAndHeightHeader}, {angleHeader}) VALUES (@{nameHeader}, @{baseAndHeightHeader}, @{angleHeader})"
            |> Sql.parameters [
                $"@{nameHeader}", Sql.string setCone.name
                $"@{baseAndHeightHeader}", Sql.int (int setCone.baseAndHeight)
                $"@{angleHeader}", Sql.decimal (decimal setCone.angle)
            ]
            |> Sql.executeNonQuery

        let insertSetCones = Set.map insertSetCone

        let getSetCones () =
            databaseConnection
            |> Sql.query $"SELECT {nameHeader}, {baseAndHeightHeader}, {angleHeader} FROM {setConesTableName}"
            |> Sql.execute (fun read -> {
                name = read.string nameHeader
                baseAndHeight = read.int baseAndHeightHeader |> uint
                angle = read.decimal angleHeader |> float
            })
            |> Set.ofList

    module CoreSkillDataDatabase =
        open FogentRoleplayLib.CoreSkillData
        open FogentRoleplayLib.SkillName
        open FogentRoleplayLib.AttributeName

        let coreSkillDataTableName = "core_skill_data"
        let skillNameHeader = "skill_name"
        let attributeNameHeader = "attribute_name"

        let initCoreSkillDataTable () =
            databaseConnection
            |> Sql.query
                $"""
                CREATE TABLE IF NOT EXISTS {coreSkillDataTableName} (
                    {skillNameHeader} VARCHAR(100) PRIMARY KEY,
                    {attributeNameHeader} VARCHAR(100) NOT NULL
                )
            """
            |> Sql.executeNonQuery
            |> function
                | affectedRows ->
                    printfn "Table %s created successfully. Rows affected: %d" coreSkillDataTableName affectedRows

        let insertCoreSkillData (coreSkillData: CoreSkillData) =
            databaseConnection
            |> Sql.query
                $"INSERT INTO {coreSkillDataTableName} ({skillNameHeader}, {attributeNameHeader}) VALUES (@{skillNameHeader}, @{attributeNameHeader})"
            |> Sql.parameters [
                $"@{skillNameHeader}", Sql.string (SkillName coreSkillData.skillName)
                $"@{attributeNameHeader}", Sql.string (AttributeName coreSkillData.attributeName)
            ]
            |> Sql.executeNonQuery

        let insertCoreSkillDataSet = Set.map insertCoreSkillData

        let getCoreSkillDataSet () : Set<CoreSkillData> =
            databaseConnection
            |> Sql.query $"SELECT {skillNameHeader}, {attributeNameHeader} FROM {coreSkillDataTableName}"
            |> Sql.execute (fun read -> {
                skillName = SkillName(read.string skillNameHeader)
                attributeName = AttributeName(read.string attributeNameHeader)
            })
            |> Set.ofSeq

    module MagicSkillDataDatabase =

        open FogentRoleplayLib.MagicSkillData
        open FogentRoleplayLib.SkillName
        open FogentRoleplayLib.DamageType

        let magicSkillDataTableName = "magic_skill_data"
        let nameHeader = "name"
        let damageTypesHeader = "damage_types"
        let isMeleeCapableHeader = "is_melee_capable"
        let isRangeCapableHeader = "is_range_capable"

        let initMagicSkillDataTable () =
            databaseConnection
            |> Sql.query
                $"""
                CREATE TABLE IF NOT EXISTS {magicSkillDataTableName} (
                    {nameHeader} VARCHAR(100) PRIMARY KEY,
                    {damageTypesHeader} TEXT[] NOT NULL,
                    {isMeleeCapableHeader} BOOLEAN NOT NULL,
                    {isRangeCapableHeader} BOOLEAN NOT NULL
                );
            """
            |> Sql.executeNonQuery
            |> function
                | affectedRows ->
                    printfn "Table %s created successfully. Rows affected: %d" magicSkillDataTableName affectedRows

        let insertMagicSkillData (magicSkillData: MagicSkillData) =
            databaseConnection
            |> Sql.query
                $"INSERT INTO {magicSkillDataTableName} ({nameHeader}, {damageTypesHeader}, {isMeleeCapableHeader}, {isRangeCapableHeader}) VALUES (@{nameHeader}, @{damageTypesHeader}, @{isMeleeCapableHeader}, @{isRangeCapableHeader})"
            |> Sql.parameters [
                $"@{nameHeader}", Sql.string (SkillName magicSkillData.name)
                $"@{damageTypesHeader}",
                Sql.stringArray (magicSkillData.damageTypes |> Set.toArray |> Array.map DamageType)
                $"@{isMeleeCapableHeader}", Sql.bool magicSkillData.isMeleeCapable
                $"@{isRangeCapableHeader}", Sql.bool magicSkillData.isRangeCapable
            ]
            |> Sql.executeNonQuery

        let insertMagicSkillDataSet = Set.map insertMagicSkillData

        let getMagicSkillDataSet (damageTypeMap: Map<string, DamageType>) : Set<MagicSkillData> =
            databaseConnection
            |> Sql.query
                $"SELECT {nameHeader}, {damageTypesHeader}, {isMeleeCapableHeader}, {isRangeCapableHeader} FROM {magicSkillDataTableName}"
            |> Sql.execute (fun read -> {
                name = SkillName(read.string nameHeader)
                damageTypes = read.stringArray damageTypesHeader |> Array.map DamageType |> Set.ofArray
                isMeleeCapable = read.bool isMeleeCapableHeader
                isRangeCapable = read.bool isRangeCapableHeader
            })
            |> Set.ofSeq

    open DamageTypesDatabase
    open EngageableOpponentsCalculationDatabase
    open CalculatedRangesDatabase
    open RangeCalculationsDatabase
    open SphereCalculationDatabase
    open ConeCalculationDatabase
    open SetSphereDatabase
    open SetConeDatabase
    open CoreSkillDataDatabase
    open MagicSkillDataDatabase
    // Init Database
    let initDatabase () =
        initDamageTypesTable ()
        initEngageableOpponentsCalculationTable ()
        initCalculatedRangesTable ()
        initRangeCalculationTable ()
        initSphereCalculationTable ()
        initConeCalculationTable ()
        initSetSphereTable ()
        initSetConeTable ()
        initCoreSkillDataTable ()
        initMagicSkillDataTable ()

open Database

open FogentRoleplayServerData

//initDatabase ()

//insertDamageTypes damageTypes
//insertEngageableOpponentsCalculations engageableOpponentsCalculations
//insertCalculatedRanges calculatedRanges
//insertRangeCalculations rangeCalculations
//SphereCalculationDatabase.insertSphereCalculations sphereCalculationSet
//ConeCalculationDatabase.insertConeCalculations coneCalculationSet
//SetSphereDatabase.insertSetSpheres setSphereSet
//SetConeDatabase.insertSetCones setConeSet
//CoreSkillDataDatabase.insertCoreSkillDataSet coreSkillDataSet
//MagicSkillDataDatabase.insertMagicSkillDataSet magicSkillDataSet


// open System.IO
// open System
// open LiteDB
// open LiteDB.FSharp

// let db =
//     let mapper = FSharpBsonMapper()
//     let connStr = "Filename=fogentData.db;mode=Exclusive"
//     new LiteDatabase(connStr, mapper)

// open FogentRoleplayLib.DamageType

// [<CLIMutable>]
// type DamageTypeDocument = { Id: int; damageType: DamageType }

// let damageTypeCollection: LiteCollection<DamageTypeDocument> =
//     db.GetCollection<DamageTypeDocument>("damageTypes")

// FogentRoleplayServerData.damageTypes
// |> Set.iter (fun dt -> damageTypeCollection.Insert({ Id = 0; damageType = dt }) |> ignore)
// |> ignore

// let allPeople = damageTypeCollection.FindAll() |> Seq.toList

open FogentRoleplayLib.SettingData

let fallenDataApi =

    let damageTypeSet = DamageTypesDatabase.getDamageTypes ()

    let engageableOpponentsCalculationSet =
        EngageableOpponentsCalculationDatabase.getEngageableOpponentsCalculations ()

    let calculatedRangeSet = CalculatedRangesDatabase.getCalculatedRanges ()
    let rangeCalculationSet = RangeCalculationsDatabase.getRangeCalculations ()
    let sphereCalculationSet = SphereCalculationDatabase.getSphereCalculations ()
    let coneCalculationSet = ConeCalculationDatabase.getConeCalculations ()
    let setSphereSet = SetSphereDatabase.getSetSpheres ()
    let setConeSet = SetConeDatabase.getSetCones ()

    fun () -> async {
        return {
            attributeNameSet = FogentRoleplayServerData.attributeNameSet
            coreSkillDataSet = FogentRoleplayServerData.coreSkillDataSet
            itemElementMap = FogentRoleplayServerData.itemStackMap
            weaponSpellSet = FogentRoleplayServerData.weaponSpellSet
            magicSystemMap = FogentRoleplayServerData.magicSystemData
            weaponSkillDataMap = FogentRoleplayServerData.weaponSkillDataMap
            effectMap = FogentRoleplayServerData.effectDataMap
            combatSpeedCalculationMap = FogentRoleplayServerData.combatSpeedCalculationMap
            carryWeightCalculationMap = FogentRoleplayServerData.carryWeightCalculationMap
            weightClassSet = FogentRoleplayServerData.weightClassSet
        }
    }

open Authorize

open FogentRoleplayLib.Character

let getCharacterList (userData: UserData) : Async<Character list> = async {
    // In here I will have to search the DB for which characters the player has access to
    return List.empty
}

let guestApi: IGuestApi = { login = login }

let userApi: IUserApi = {
    getInitCharacterData = fallenDataApi
    getCharacterList = getCharacterList
}

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue userApi
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