module AttributeAndCoreSkills

open FogentRoleplayLib.AttributeAndCoreSkills
open FogentRoleplayLib.Skill

type Msg =
    | AttributeMsg of AttributeStat.Msg * DicePoolCalculationData
    | CoreSkillListMsg of CoreSkillList.Msg

// let init () =

let update msg model =
    match msg with
    | AttributeMsg (msg, dicePoolCalculationData) ->
        let newAttribute = AttributeStat.update msg model.attribute
        { model with 
            attribute = newAttribute
            coreSkills = 
                CoreSkillList.update
                    (CoreSkillList.Msg.CalculateCoreSkillDicePools dicePoolCalculationData)
                    model.coreSkills
        }
    | CoreSkillListMsg msg ->
        { model with coreSkills = CoreSkillList.update msg model.coreSkills }
