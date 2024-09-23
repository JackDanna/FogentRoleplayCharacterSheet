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

let addNewCharacter username = async { return LiteDBDatabase.addNewCharacter (getInitSettingDataFromCSV ()) username }
let getInitSettingData () = async { return getInitSettingDataFromCSV () }

let getCharacterList (userData: UserData) = async {
    // In here I will have to search the DB for which characters the player has access to
    return
        userData.username
        |> usernameToIdUser
        |> function
            | Some idUser -> getCharactersForUser idUser.Id
            | None -> Seq.empty
        |> List.ofSeq
}

let userApi: IUserApi = {
    addNewCharacter = addNewCharacter
    getInitSettingData = getInitSettingData
    getIdCharacterList = getCharacterList
}

open Authorize
let guestApi: IGuestApi = { login = login }

let create api =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue api
    //|> Remoting.withErrorHandler ErrorHandling.errorHandler
    |> Remoting.buildHttpHandler