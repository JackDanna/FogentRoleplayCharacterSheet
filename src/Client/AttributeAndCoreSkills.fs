module AttributeAndCoreSkills

open FogentRoleplayLib.AttributeAndCoreSkills
open FogentRoleplayLib.AttributeAndCoreSkillsData
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | AttributeMsg of Attribute.Msg
    | CoreSkillListMsg of CoreSkills.Msg
    | CalculateDicePool of DicePoolCalculationData

let init (attributeAndCoreSkillsData: AttributeAndCoreSkillsData) (dicePoolCalculationData: DicePoolCalculationData) =

    {
        attributeStat = Attribute.init attributeAndCoreSkillsData.governingAttributeName
        coreSkills =
            CoreSkills.init
                attributeAndCoreSkillsData.coreSkillNameSet
                attributeAndCoreSkillsData.governingAttributeName
                dicePoolCalculationData
    }

let update msg model =
    match msg with
    | AttributeMsg msg -> {
        model with
            attributeStat = Attribute.update msg model.attributeStat
      }
    | CoreSkillListMsg msg -> {
        model with
            coreSkills = CoreSkills.update msg model.coreSkills
      }
    | CalculateDicePool dicePoolCalculationData ->

        {
            model with
                coreSkills =
                    CoreSkills.update (CoreSkills.CalculateCoreSkillDicePools(dicePoolCalculationData)) model.coreSkills
        }

open Feliz
open Feliz.Bulma

let view model dispatch =
    Bulma.box [
        Attribute.view model.attributeStat (AttributeMsg >> dispatch)
        CoreSkills.view model.coreSkills (CoreSkillListMsg >> dispatch)
    ]