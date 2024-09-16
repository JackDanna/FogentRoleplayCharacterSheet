module LiteDBDatabase

open System.IO
open System
open LiteDB
open LiteDB.FSharp

let db =
    let mapper = FSharpBsonMapper()
    let connStr = "Filename=fogentData.db;mode=Exclusive"
    new LiteDatabase(connStr, mapper)

open FogentRoleplayLib.DamageType

[<CLIMutable>]
type DamageTypeDocument = { Id: int; damageType: DamageType }

let damageTypeCollection: LiteCollection<DamageTypeDocument> =
    db.GetCollection<DamageTypeDocument>("damageTypes")

CsvDatabase.damageTypes
|> Set.iter (fun dt -> damageTypeCollection.Insert({ Id = 0; damageType = dt }) |> ignore)
|> ignore

let allPeople = damageTypeCollection.FindAll() |> Seq.toList
