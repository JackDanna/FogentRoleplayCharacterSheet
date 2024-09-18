[<RequireQualifiedAccess>]
module Overview

open Elmish

open Fable.Remoting.Client
open Shared

type State = {
    User: Shared.UserData
    CharacterList: CharacterList.Model
}

let getUserApi token =
    Remoting.createApi ()
    |> Remoting.withAuthorizationHeader $"Bearer {token}"
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IUserApi>

type Msg = CharacterListMsg of CharacterList.Msg

let init (user: Shared.UserData) =
    let characterListModel, characterListCmd = CharacterList.init user

    {
        User = user
        CharacterList = characterListModel
    },
    Cmd.map CharacterListMsg characterListCmd


let update (msg: Msg) (state: State) : State * Cmd<Msg> =

    let userApi = getUserApi state.User.token

    match msg with
    | CharacterListMsg(characterListMsg: CharacterList.Msg) ->
        match characterListMsg with
        | CharacterList.Msg.AddNewCharacter _ ->

            let characterListModel, characterListCmd =
                CharacterList.update
                    (userApi.getInitSettingData |> Some |> CharacterList.Msg.AddNewCharacter)
                    state.CharacterList

            {
                state with
                    CharacterList = characterListModel
            },
            Cmd.map CharacterListMsg characterListCmd

        | _ ->
            let characterListModel, characterListCmd =
                CharacterList.update characterListMsg state.CharacterList

            {
                state with
                    CharacterList = characterListModel
            },
            Cmd.map CharacterListMsg characterListCmd

open Feliz.Bulma

let view (model: State) dispatch =
    Bulma.hero [
        hero.isFullHeight
        color.isDanger

        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl
                "https://www.onlygfx.com/wp-content/uploads/2015/12/simple-old-paper-1-transparent.jpg"
            style.backgroundPosition "no-repeat center center fixed"
        ]

        prop.children [

            Bulma.heroHead [
                Bulma.navbar [
                    color.isPrimary
                    prop.children [
                        Bulma.navbarItem.div [
                            Bulma.title.h3 [ prop.text "Fallen"; prop.style [ style.fontFamily "PT Serif Caption" ] ]
                        ]
                    ]
                ]
            ]

            Bulma.heroBody [ CharacterList.view model.CharacterList (CharacterListMsg >> dispatch) ]
        ]
    ]