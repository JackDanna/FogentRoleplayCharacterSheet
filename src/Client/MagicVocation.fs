module MagicVocation

open FogentRoleplayLib.MagicVocation

type Msg =
    | MundaneVocationMsg of MundaneVocation.Msg
    | MagicVocationSkillListMsg of MagicSkillList.Msg
    | SetCurrentMagicResource of uint

// let init () : MagicVocation = {

// }

let update msg (model: MagicVocation) =
    match msg with
    | MundaneVocationMsg msg -> {
        model with
            mundaneVocation = MundaneVocation.update msg model.mundaneVocation
      }
    | MagicVocationSkillListMsg msg -> {
        model with
            magicVocationSkillList = MagicSkillList.update msg model.magicVocationSkillList
      }
    | SetCurrentMagicResource newCurrentMagicResource -> {
        model with
            currentMagicResource =
                if newCurrentMagicResource > model.magicResourceCap then
                    model.magicResourceCap
                else
                    newCurrentMagicResource
      }

open Feliz
open Feliz.Bulma

let view attributeNameSet (weaponSkillNames) (model: MagicVocation) dispatch =
    [
        Bulma.columns [
            Bulma.column [ prop.text model.magicSystem.resourceName ]
            Bulma.column [
                Bulma.input.number [
                    prop.min 0
                    prop.value (int model.currentMagicResource)
                    prop.onChange (fun (num: int) -> dispatch (SetCurrentMagicResource(uint num)))
                ]
            ]
            Bulma.column [ sprintf "Max: %d" model.currentMagicResource |> prop.text ]
        ]
    ]
    |> List.append (
        MundaneVocation.view attributeNameSet weaponSkillNames model.mundaneVocation (MundaneVocationMsg >> dispatch)
    )