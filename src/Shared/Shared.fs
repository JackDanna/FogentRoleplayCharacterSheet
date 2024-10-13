namespace Shared

open FogentRoleplayLib.SettingData
open FogentRoleplayLib.Character

type Login = { userName: string; password: string }

//
// type IdEntity<'T> = { Id: int; Entity: 'T }

[<CLIMutable>]
type IdUser = { Id: int; Login: Login }

type JWT = string
type Username = string
type UserData = { username: Username; token: JWT }

type LoginResult =
    | UsernameOrPasswordIncorrect
    | LoggedIn of UserData

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IGuestApi = { login: Login -> Async<LoginResult> }

type IUserApi = {
    addNewCharacter: Username -> Async<Character List>
    getInitSettingData: unit -> Async<SettingData>
    getIdCharacterList: Username -> Async<Character List>
    updateIdCharacter: Username -> Character -> Async<unit>
}