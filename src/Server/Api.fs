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


open FogentRoleplayLib.SettingData
open CsvDatabase

let getInitSettingData () = async { return getInitSettingDataFromCSV () }

open FogentRoleplayLib.Character

let getCharacterList (userData: UserData) : Async<Character list> = async {
    // In here I will have to search the DB for which characters the player has access to
    return List.empty
}

let userApi: IUserApi = {
    getInitSettingData = getInitSettingData
    getCharacterList = getCharacterList
}

open Authorize
let guestApi: IGuestApi = { login = login }

let create api =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue api
    //|> Remoting.withErrorHandler ErrorHandling.errorHandler
    |> Remoting.buildHttpHandler