module Index

open Elmish

type Model = { characterList: CharacterList.Model }

type Msg = CharacterListMsg of CharacterList.Msg

let init (userData) : Model * Cmd<Msg> =
    let characterList, characterListCmd = CharacterList.init (userData)
    { characterList = characterList }, characterListCmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | CharacterListMsg characterListMsg ->
        let characterList, characterListCmd =
            CharacterList.update characterListMsg model.characterList

        {
            model with
                characterList = characterList
        },
        Cmd.map Msg.CharacterListMsg characterListCmd

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
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

            Bulma.heroBody [ CharacterList.view model.characterList (CharacterListMsg >> dispatch) ]
        ]
    ]