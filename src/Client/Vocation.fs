module vocationList

open FogentRoleplayLib.Vocation
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.DicePool

type Msg =
    | SetName of string
    | ZeroToFiveMsg of ZeroToFive.Msg
    | ToggleGoveringAttribute of AttributeName

let init () : Vocation = {
    name = ""
    level = ZeroToFive.init ()
    governingAttributeNames = Set.empty
    dicePool = emptyDicePool
    vocationSkillList = VocationSkillList.init ()
}

let update msg model =
    match msg with
    | SetName newName -> { model with name = newName }
    | ZeroToFiveMsg msg -> {
        model with
            level = ZeroToFive.update msg model.level
      }
    | ToggleGoveringAttribute newAttributeName -> {
        model with
            governingAttributeNames = toggleAttributeNameSet model.governingAttributeNames newAttributeName
      }