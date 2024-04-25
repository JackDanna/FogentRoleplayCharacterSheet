module CharacterInformation

open FogentRoleplayLib.CharacterInformation

type Msg =
    | SetNotes of string
    | SetAppearance of string
    | SetDisposition of string
    | SetBeliefsAndMorality of string
    | SetGoalsAndAspirations of string
    | SetBackstory of string

let init () = {
    notes = ""
    appearance = ""
    disposition = ""
    beliefsAndMorality = ""
    goalsAndAspirations = ""
    backstory = ""
}

let update (msg: Msg) (model: CharacterInformation) =
    match msg with
    | SetNotes newText -> { model with notes = newText }
    | SetAppearance newText -> { model with appearance = newText }
    | SetDisposition newText -> { model with disposition = newText }
    | SetBeliefsAndMorality newText -> {
        model with
            beliefsAndMorality = newText
      }
    | SetGoalsAndAspirations newText -> {
        model with
            goalsAndAspirations = newText
      }
    | SetBackstory newText -> { model with backstory = newText }


open Feliz
open Feliz.Bulma

let view (model: CharacterInformation) (dispatch: Msg -> unit) =

    let characterInformationTextArea (labelName: string) dispatchMsg =
        Bulma.container [
            Html.label [ prop.text labelName ]
            Bulma.textarea [
                prop.placeholder $"Enter {labelName}..."
                prop.onTextChange (fun text -> dispatch (dispatchMsg text))
            ]
        ]

    Bulma.container [
        characterInformationTextArea "Backstory:" SetBackstory
        characterInformationTextArea "Notes:" SetNotes
        characterInformationTextArea "Beliefs/Morality:" SetBeliefsAndMorality
        characterInformationTextArea "Goals/Aspirations:" SetGoalsAndAspirations
        characterInformationTextArea "Disposition:" SetDisposition
    ]