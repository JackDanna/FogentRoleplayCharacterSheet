module LiteDBDatabase

open System.IO
open System
open System.Collections.Generic
open LiteDB
open LiteDB.FSharp
open Shared

open FogentRoleplayLib.Character

// [<CLIMutableAttribute>]
// type Mut_IdUser = Shared.IdUser

// [<CLIMutableAttribute>]
// type Mut_IdCharacter = Shared.IdCharacter

let db =
    let mapper = FSharpBsonMapper()
    let connStr = "Filename=fogentData.db;mode=Exclusive"
    new LiteDatabase(connStr, mapper)

let collectionFromDB<'T> = db.GetCollection<'T>(typeof<'T>.Name)

// let insertEntity (entity: 'T) =
//     // Since Id is set to 0, inserted entities will be placed according to auto-incrementation
//     let idEntity = { Id = 0; Entity = entity }
//     collectionFromDB.Insert(idEntity) |> ignore
//     idEntity

// let findEntity entity = collectionFromDB.FindById entity.Id

// let insertEntities entities =
//     entities |> Seq.map (fun entity -> insertEntity entity)

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

let insertNewUser (user: Login) =
    let idEntity = { Id = 0; Login = user }
    collectionFromDB.Insert(idEntity) |> ignore
    idEntity

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

let insertCharacter userId (character: Character) =
    let idCharacter = createAutoIncrementedIdCharacter character
    collectionFromDB.Insert(idCharacter) |> ignore
    grantAccess userId idCharacter.Id
    idCharacter

let getCharactersForUser userId =
    userCharacterAccesses.Find(fun uca -> uca.UserId = userId)
    |> Seq.map (fun uca -> characters.FindById(BsonValue(uca.CharacterId)))

let getUsersForCharacter characterId =
    userCharacterAccesses.Find(fun uca -> uca.CharacterId = characterId)
    |> Seq.map (fun uca -> users.FindById(BsonValue(uca.UserId)))

let usernameToIdUser (username: Username) =
    users.Find(fun idUser -> idUser.Login.userName = username) |> Seq.tryHead

let addNewCharacter settingData username =

    match usernameToIdUser username with
    | Some idUser ->
        settingData
        |> FogentRoleplayLib.Character.init
        |> insertCharacter idUser.Id
        |> ignore

        getCharactersForUser idUser.Id
    | None -> Seq.empty
    |> List.ofSeq

let isValidUserLogin login =

    users.Find(
        Query.And(
            Query.EQ("Login.userName", BsonValue(login.userName)),
            Query.EQ("Login.password", BsonValue(login.password))
        )
    )
    |> Seq.tryHead
    |> (function
    | Some _ -> true
    | None ->
        insertNewUser login // TESTING, REMOVE ASAP: This automatically creates a user if it doesn't exists,
        false)