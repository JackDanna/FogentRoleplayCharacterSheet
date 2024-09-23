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
type IdEntity<'T> = { Id: int; Entity: 'T }

let collectionFromDB<'T> = db.GetCollection<IdEntity<'T>>(typeof<'T>.Name)

let insertEntity (entity: 'T) =
    // Since Id is set to 0, inserted entities will be placed according to auto-incrementation
    collectionFromDB.Insert({ Id = 0; Entity = entity }) |> ignore

let findEntity entity = collectionFromDB.FindById entity.Id

let insertEntities entities =
    entities |> Seq.map (fun entity -> insertEntity entity)

open CsvDatabase