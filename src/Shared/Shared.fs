namespace Shared

open FogentRoleplayLib.SettingData
open FogentRoleplayLib.Character

type Login = { userName: string; password: string }

//
// type IdEntity<'T> = { Id: int; Entity: 'T }

[<CLIMutable>]
type IdUser = { Id: int; Login: Login }

[<AutoOpen>]
module IdCharacter =
    [<CLIMutable>]
    type IdCharacter = { Id: int; Character: Character }

    let createAutoIncrementedIdCharacter character = { Id = 0; Character = character }

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
    addNewCharacter: Username -> Async<IdCharacter List>
    getInitSettingData: unit -> Async<SettingData>
    getIdCharacterList: UserData -> Async<IdCharacter List>
}