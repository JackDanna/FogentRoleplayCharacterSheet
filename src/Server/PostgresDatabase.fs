module PostgresDatabase

open Npgsql
open Npgsql.FSharp

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
    open FogentRoleplayLib.CalculatedRange
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
    open FogentRoleplayLib.RangeCalculation
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
    open FogentRoleplayLib.SphereCalculation

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
    open FogentRoleplayLib.ConeCalculation

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
    open FogentRoleplayLib.SetSphere

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
    open FogentRoleplayLib.SetCone // Assuming this is where SetCone is defined

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
            | affectedRows -> printfn "Table %s created successfully. Rows affected: %d" setConesTableName affectedRows

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
            $"@{damageTypesHeader}", Sql.stringArray (magicSkillData.damageTypes |> Set.toArray |> Array.map DamageType)
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