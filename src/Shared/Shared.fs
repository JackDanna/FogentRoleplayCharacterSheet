namespace Shared

open FogentRoleplayLib.Character
open FogentRoleplayLib.Setting

type Username = string

type Login = { username: Username; password: string }

[<CLIMutable>]
type IdUser = { Id: int; Login: Login }

type JWT = string

type UserData = { username: Username; token: JWT }

type LoginResult =
    | UsernameOrPasswordIncorrect
    | LoggedIn of UserData

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IGuestApi = { login: Login -> Async<LoginResult> }

type IUserApi = {
    addNewCharacterApi: Username -> int -> Async<Result<Character, string>>
    getOwnedSettingApi: Username -> Async<Setting seq>
    updateCharacterApi: Username -> int -> Character -> Async<Result<unit, string>>
}