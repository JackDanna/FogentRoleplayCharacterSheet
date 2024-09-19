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

[<CLIMutable>]
type IdWrapper<'T> = 
    { 
        Id: int
        Entity: 'T 
    }

let collectionFromDB<'T> =
    db.GetCollection<IdWrapper<'T>>(typeof<'T>.Name)

let insertEntity (entity: 'T) (collection:LiteCollection<IdWrapper<'T>>) =
    collection.Insert({ Id = 0; Entity = entity })

let insertEntities (entities: 'T seq) =

    entities
    |> Seq.map (fun entity -> insertEntity entity collectionFromDB<'T>)
    |> ignore

// DamageType
open FogentRoleplayLib.DamageType

let idDamageTypes: LiteCollection<IdWrapper<DamageType>> =
    collectionFromDB<DamageType>

// EngageableOpponents

open FogentRoleplayLib.EngageableOpponents

let idEngageableOpponentsCalculations =
    db.GetCollection<IdWrapper<EngageableOpponents>>(engageableOpponentsCalculationTableName)

// CalculatedRange

open FogentRoleplayLib.CalculatedRange

let idCalculatedRanges =
    db.GetCollection<IdWrapper<CalculatedRange>>(calculatedRangeTableName)

// RangeCalculation

open FogentRoleplayLib.RangeCalculation

let idRangeCalculations =
    db.GetCollection<IdWrapper<RangeCalculation>>(rangeCalculationTableName)

// SphereCalculation

open FogentRoleplayLib.SphereCalculation

let idSphereCalculations =
    db.GetCollection<IdWrapper<SphereCalculation>>(sphereCalculationTableName)

// ConeCalculation

open FogentRoleplayLib.ConeCalculation

let idConeCalculation =
    db.GetCollection<IdWrapper<ConeCalculation>>(coneCalculationTableName)

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

//SetCone

open FogentRoleplayLib.SetCone

[<CLIMutable>]
type IdSetCone = { Id: int; setCone: SetCone }

let idSetCone = db.GetCollection<IdSetCone>(setConeTableName)

let insertSetCone scs =
    scs |> Seq.map (fun sc -> idSetCone.Insert({ Id = 0; setCone = sc })) |> ignore

// AreaOfEffect

// ResourceClass

open FogentRoleplayLib.ResourceName

[<CLIMutable>]
type IdResourceName = { Id: int; resourceName: ResourceName }

let idResourceNames = db.GetCollection<IdResourceName>(resourceNameTableName)

let insertResourceNames rns =
    rns
    |> Seq.map (fun rn -> idResourceNames.Insert({ Id = 0; resourceName = rn }))
    |> ignore

// Attribute

// CoreSkillData

open FogentRoleplayLib.CoreSkillData

[<CLIMutable>]
type IdCoreSkillData = {
    Id: int
    coreSkillData: CoreSkillData
}

let idCoreSkillDatas = db.GetCollection<IdCoreSkillData>(coreSkillDataTableName)

let insertCoreSkillData csds =
    csds
    |> Seq.map (fun csd -> idCoreSkillDatas.Insert({ Id = 0; coreSkillData = csd }))
    |> ignore

// MagicSkillData

open FogentRoleplayLib.MagicSkillData

[<CLIMutable>]
type IdMagicSkillData = {Id: int; magicSkillData = }


let allPeople = idDamageTypes.FindAll() |> Seq.toList