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

let collectionFromDB<'T> = db.GetCollection<'T>(typeof<'T>.Name)

let insertEntity (entity: 'T) =
    // Since Id is set to 0, inserted entities will be placed according to auto-incrementation
    let idEntity = { Id = 0; Entity = entity }
    collectionFromDB.Insert(idEntity) |> ignore
    idEntity

let findEntity entity = collectionFromDB.FindById entity.Id

let insertEntities entities =
    entities |> Seq.map (fun entity -> insertEntity entity)

open CsvDatabase

open Shared

type IdUser = IdEntity<Login>
type IdCharacter = IdEntity<FogentRoleplayLib.Character.Character>

type UserCharacterAccess = {
    UserId: int
    CharacterId: int
//AccessGranted: System.DateTime
//AccessType: string
}

let users = collectionFromDB<IdUser>
let characters = collectionFromDB<IdCharacter>
let userCharacterAccesses = collectionFromDB<UserCharacterAccess>

//users.EnsureIndex(fun (u: IdUser) -> u.Entity.userName) |> ignore

let insertNewUser (user: Login) = insertEntity user

let insertCharacter character = insertEntity character

let grantAccess userId characterId =
    userCharacterAccesses.Insert(
        {
            UserId = userId
            CharacterId = characterId
        //AccessGrantedDate = System.DateTime.UtcNow
        //AccessType = accessType
        }
    )
    |> ignore

let addNewCharacter settingData userId =
    let character = FogentRoleplayLib.Character.init settingData
    let idCharacter = insertEntity character

    grantAccess userId idCharacter.Id

let getCharactersForUser userId =
    userCharacterAccesses.Find(fun uca -> uca.UserId = userId)
    |> Seq.map (fun uca -> characters.FindById(BsonValue(uca.CharacterId)))
    |> Seq.toArray

// Function to get all users who have access to a character
let getUsersForCharacter characterId =
    userCharacterAccesses.Find(fun uca -> uca.CharacterId = characterId)
    |> Seq.map (fun uca -> users.FindById(BsonValue(uca.UserId)))
    |> Seq.toArray