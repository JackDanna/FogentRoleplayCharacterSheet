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
type IdWrapper<'T> = { Id: int; Entity: 'T }

let collectionFromDB<'T> = db.GetCollection<IdWrapper<'T>>(typeof<'T>.Name)

let insertEntity (entity: 'T) (collection: LiteCollection<IdWrapper<'T>>) =
    collection.Insert({ Id = 0; Entity = entity })

let insertEntities (entities: 'T seq) =
    entities
    |> Seq.map (fun entity -> insertEntity entity collectionFromDB<'T>)
    |> ignore

open CsvDatabase



// Characters will need to be there own talbe with multiple data