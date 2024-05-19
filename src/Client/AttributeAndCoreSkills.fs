module AttributeAndCoreSkills

open FogentRoleplayLib.AttributeAndCoreSkills
open FogentRoleplayLib.AttributeAndCoreSkillsData
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.Effect

type Msg =
    | AttributeMsg of Attribute.Msg * option<Effect List>
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
    | AttributeMsg(msg, Some effects) ->
        let newAttribute = Attribute.update msg model.attributeStat

        {
            model with
                attributeStat = newAttribute
                coreSkills =
                    CoreSkills.update
                        (CoreSkills.CalculateCoreSkillDicePools {
                            effects = effects
                            attributes = Set.ofList [ newAttribute ] // Here I'm only sending down the attribute that effects the core skill, eventually I'd like to change it so CoreSKill data for this example doesnt require a Set of attributes
                        })
                        model.coreSkills
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
    | _ -> model

open Feliz
open Feliz.Bulma

let view model dispatch =
    Bulma.box [
        Attribute.view model.attributeStat ((fun msg -> AttributeMsg(msg, None)) >> dispatch)
        CoreSkills.view model.coreSkills (CoreSkillListMsg >> dispatch)
    ]