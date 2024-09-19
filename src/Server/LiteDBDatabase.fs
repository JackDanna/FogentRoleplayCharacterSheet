module LiteDBDatabase

open System.IO
open System
open System.Collections.Generic
open LiteDB
open LiteDB.FSharp
open DatabaseUtils

let db =
    let mapper = FSharpBsonMapper()
    let connStr = "Filename=fogentData.db;mode=Exclusive"
    new LiteDatabase(connStr, mapper)

// DamageType

open FogentRoleplayLib.DamageType

[<CLIMutable>]
type IdDamageType = { Id: int; damageType: DamageType }

let idDamageTypes: LiteCollection<IdDamageType> =
    db.GetCollection<IdDamageType>(damagageTypeTableName)

let insertDamageTypes (damageTypes: DamageType seq) =
    damageTypes
    |> Seq.map (fun damageType -> idDamageTypes.Insert({ Id = 0; damageType = damageType }))
    |> ignore

// EngageableOpponents

open FogentRoleplayLib.EngageableOpponents

[<CLIMutable>]
type IdEngageableOpponentsCalculation = {
    Id: int
    engageableOpponentsCalculation: EngageableOpponentsCalculation
}

let idEngageableOpponentsCalculations =
    db.GetCollection<IdEngageableOpponentsCalculation>(engageableOpponentsCalculationTableName)

let insertEngageableOpponentsCalculations eocs =
    eocs
    |> Seq.map (fun eoc ->
        idEngageableOpponentsCalculations.Insert(
            {
                Id = 0
                engageableOpponentsCalculation = eoc
            }
        ))
    |> ignore

// CalculatedRange

open FogentRoleplayLib.CalculatedRange

[<CLIMutable>]
type IdCalculatedRange = {
    Id: int
    calculatedRange: CalculatedRange
}

let idCalculatedRanges =
    db.GetCollection<IdCalculatedRange>(calculatedRangeTableName)

let insertCalculatedRange crs =
    crs
    |> Seq.map (fun cr -> idCalculatedRanges.Insert({ Id = 0; calculatedRange = cr }))
    |> ignore

// RangeCalculation

open FogentRoleplayLib.RangeCalculation

[<CLIMutable>]
type IdRangeCalculation = {
    Id: int
    rangeCalculation: RangeCalculation
}

let idRangeCalculations =
    db.GetCollection<IdRangeCalculation>(rangeCalculationTableName)

let insertRangeCalculation rcs =
    rcs
    |> Seq.map (fun rc -> idRangeCalculations.Insert({ Id = 0; rangeCalculation = rc }))
    |> ignore

// SphereCalculation

open FogentRoleplayLib.SphereCalculation

[<CLIMutable>]
type IdSphereCalculation = {
    Id: int
    sphereCalculation: SphereCalculation
}

let idSphereCalculations =
    db.GetCollection<IdSphereCalculation>(sphereCalculationTableName)

let insertSphereCalculation scs =
    scs
    |> Seq.map (fun sc -> idSphereCalculations.Insert({ Id = 0; sphereCalculation = sc }))
    |> ignore

// ConeCalculation

open FogentRoleplayLib.ConeCalculation

[<CLIMutable>]
type IdConeCalculation = {
    Id: int
    coneCalculation: ConeCalculation
}

let idConeCalculation =
    db.GetCollection<IdConeCalculation>(coneCalculationTableName)

let insertConeCalculationFromCSV ccs =
    ccs
    |> Seq.map (fun cc -> idConeCalculation.Insert({ Id = 0; coneCalculation = cc }))

// SetSphere

open FogentRoleplayLib.SetSphere

[<CLIMutable>]
type IdSetSphere = { Id: int; setSphere: SetSphere }

let idSetSphere = db.GetCollection<IdSetSphere>(setSphereTableName)

let insertSetSphere sss =
    sss
    |> Seq.map (fun ss -> idSetSphere.Insert({ Id = 0; setSphere = ss }))
    |> ignore


let allPeople = idDamageTypes.FindAll() |> Seq.toList