namespace Shared

open FogentRoleplayLib.SettingData
open FogentRoleplayLib.Character

type JWT = string

type UserData = { username: string; token: JWT }

type Login = { userName: string; password: string }

type LoginResult =
    | UsernameOrPasswordIncorrect
    | LoggedIn of UserData

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IGuestApi = { login: Login -> Async<LoginResult> }

type IUserApi = {
    getInitSettingData: unit -> Async<SettingData>
    getCharacterList: UserData -> Async<Character List>
}