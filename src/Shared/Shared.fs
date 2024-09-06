namespace Shared

open FogentRoleplayLib.SettingData
open FogentRoleplayLib.Character


type JWT = string


type UserData = {
    id: int
    username: string
    token: JWT
//character: Character list
}

type Login = { userName: string; password: string }

type LoginResult =
    | UsernameOrPasswordIncorrect
    | LoggedIn of UserData

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IFogentRoleplayDataApi = {
    getInitData: unit -> Async<SettingData>
    login: Login -> Async<LoginResult>
    getCharacterList: UserData -> Async<Character List>
}