module LiteDBDatabase

open System.IO
open System
open System.Collections.Generic
open LiteDB
open LiteDB.FSharp

let db =
    let mapper = FSharpBsonMapper()
    let connStr = "Filename=fogentData.db;mode=Exclusive"
    new LiteDatabase(connStr, mapper)

// DamageType

open FogentRoleplayLib.DamageType

[<CLIMutable>]
type IdDamageType = { Id: int; damageType: DamageType }

let damageTypeCollection: LiteCollection<IdDamageType> =
    db.GetCollection<IdDamageType>("damageTypes")

let insertDamageTypesFromCSV (damageTypes: DamageType seq) =
    damageTypes
    |> Seq.map (fun damageType -> damageTypeCollection.Insert({ Id = 0; damageType = damageType }))
    |> ignore

// EngageableOpponents

open FogentRoleplayLib.EngageableOpponents

[<CLIMutable>]
type IdEngageableOpponentsCalculation = {
    Id: int
    engageableOpponentsCalculation: EngageableOpponentsCalculation
}

let idEngageableOpponentsCalculation =
    db.GetCollection<IdEngageableOpponentsCalculation>("engageableOpponentsCalculations")

let insertEngageableOpponentsCalculation eocs =
    eocs
    |> Seq.map (fun eoc ->
        idEngageableOpponentsCalculation.Insert(
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

let idCalculatedRange = db.GetCollection<IdCalculatedRange>("calculatedRanges")

let insertCalculatedRangeFromCSV crs =
    crs
    |> Seq.map (fun cr -> idCalculatedRange.Insert({ Id = 0; calculatedRange = cr }))
    |> ignore

// RangeCalculation

open FogentRoleplayLib.RangeCalculation

[<CLIMutable>]
type IdRangeCalculation = {
    Id: int
    rangeCalculation: RangeCalculation
}

let idRangeCalculation = db.GetCollection<IdRangeCalculation>("rangeCalculation")

let insertRangeCalculationFromCSV rcs =
    rcs
    |> Seq.map (fun rc -> idRangeCalculation.Insert({ Id = 0; rangeCalculation = rc }))
    |> ignore

// SphereCalculation

open FogentRoleplayLib.SphereCalculation

[<CLIMutable>]
type IdSphereCalculation = {
    Id: int
    sphereCalculation: SphereCalculation
}

let idSphereCalculations =
    db.GetCollection<IdSphereCalculation>("sphereCalculations")

let insertSphereCalculationFromCSV scs =
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

let idConeCalculation = db.GetCollection<IdConeCalculation>("coneCalculations")

let insertConeCalculationFromCSV ccs =
    ccs
    |> Seq.map (fun cc -> idConeCalculation.Insert({ Id = 0; coneCalculation = cc }))

// SetSphere

open FogentRoleplayLib.SetSphere

[<CLIMutable>]
type IdSetSphere = { Id: int; setSphere: SetSphere }

let idSetSphere = db.GetCollection<IdSetSphere>("sphereCalculations")

let insertSetSphereFromCSV sss =
    sss
    |> Seq.map (fun ss -> idSetSphere.Insert({ Id = 0; setSphere = ss }))
    |> ignore


let allPeople = damageTypeCollection.FindAll() |> Seq.toList