module Api

//open Azure.Data.Tables
//open Azure.Storage.Blobs
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
//open Microsoft.AspNetCore.Http
//open SAFE
open Shared
//open Storage
//open System

open CsvDatabase
open LiteDBDatabase

let addNewCharacterApi username settingId = async {
    return LiteDBDatabase.LiteDbTryInserts.insertNewCharacterInSettingForUser username settingId
}

let getOwnedSettingsApi username = async {
    return
        username
        |> tryUsernameToUser
        |> function
            | Some idUser -> userIdToOwnedSettings idUser.Id
            | None -> Seq.empty
}

let updateCharaacterApi username settingId character = async {
    return LiteDBDatabase.LiteDbTryUpdates.updateCharacter username settingId character
}

let userApi: IUserApi = {
    addNewCharacterApi = addNewCharacterApi
    getOwnedSettingApi = getOwnedSettingsApi
    updateCharacterApi = updateCharaacterApi
}

open Authorize
let guestApi: IGuestApi = { login = login }

let create api =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue api
    //|> Remoting.withErrorHandler ErrorHandling.errorHandler
    |> Remoting.buildHttpHandler