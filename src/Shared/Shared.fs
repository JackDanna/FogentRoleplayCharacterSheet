namespace Shared

open FogentRoleplayLib.SettingData

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName



type IFogentRoleplayDataApi = {
    getInitData: unit -> Async<SettingData>
}