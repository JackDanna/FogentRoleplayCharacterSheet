[<RequireQualifiedAccess>]
module Overview

open Elmish
open Feliz

type State = {
    User: Shared.UserData.UserData
    CharacterList: CharacterList.Model
}

type Msg = CharacterListMsg of CharacterList.Msg

let init (user: Shared.UserData.UserData) =
    let characterListModel, characterListCmd = CharacterList.init user

    {
        User = user
        CharacterList = characterListModel
    },
    Cmd.map CharacterListMsg characterListCmd

let update (msg: Msg) (state: State) : State * Cmd<Msg> =

    match msg with
    | CharacterListMsg msg ->
        let characterListModel, characterListCmd =
            CharacterList.update msg state.CharacterList

        {
            state with
                CharacterList = characterListModel
        },
        Cmd.map CharacterListMsg characterListCmd

// let centered (children: ReactElement list) =
//     Html.div [
//         prop.style [
//             style.margin.auto
//             style.textAlign.center
//             style.padding 20
//             style.width (length.percent 100)
//         ]

//         prop.children children
//     ]

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