module Skill

open FogentRoleplayLib.Skill
open FogentRoleplayLib.DicePool
open FogentRoleplayLib.ZeroToFive
open FogentRoleplayLib.Neg1To5
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.AttributeName

type Msg =
    | ModifySkillLevel of Neg1To5.Msg * option<ZeroToFive> * option<AttributeName Set> * option<DicePoolCalculationData>
    | CalculateDicePool of AttributeName Set * DicePoolCalculationData
    | CheckIfLevelCapExceeded of ZeroToFive * AttributeName Set * DicePoolCalculationData

let init name governingAttributeSet effects =
    let level = Neg1To5.init ()

    {
        name = name
        level = level
        dicePool = calculateSkillDicePool name level governingAttributeSet effects
    }

let update msg (model: Skill) =

    match msg with
    | ModifySkillLevel(neg1To5Msg, levelCapOption, Some governingAttributeNameSet, Some dicePoolCalculationData) ->
        let newLevel = Neg1To5.update neg1To5Msg model.level

        {
            model with
                level =
                    match levelCapOption with
                    | Some levelCap ->
                        if (zeroToFiveToInt levelCap) < (neg1To5ToInt newLevel) then
                            model.level
                        else
                            newLevel
                    | None -> newLevel
                dicePool = calculateSkillDicePool model.name newLevel governingAttributeNameSet dicePoolCalculationData
        }
    | CalculateDicePool(governingAttributeSet, dicePoolCalculationData) ->

        {
            model with
                dicePool = calculateSkillDicePool model.name model.level governingAttributeSet dicePoolCalculationData
        }
    | CheckIfLevelCapExceeded(levelCap, governingAttributeSet, dicePoolCalculationData) ->
        if (zeroToFiveToInt levelCap) < (neg1To5ToInt model.level) then

            let convertedLevelCap = levelCap |> zeroToFiveToNeg1To5

            {
                model with
                    level = convertedLevelCap
                    dicePool =
                        calculateSkillDicePool
                            model.name
                            convertedLevelCap
                            governingAttributeSet
                            dicePoolCalculationData
            }
        else
            model

    | _ -> model



open Feliz
open Feliz.Bulma

let view (model: Skill) dispatch disableChangeLevel governingSkillColumn =
    [ Bulma.column [ prop.text model.name ] ]
    @ match governingSkillColumn with
      | Some column -> column |> List.singleton
      | None -> List.Empty
    @ [
        Bulma.column [
            Neg1To5.view
                model.level
                ((fun msg -> ModifySkillLevel(msg, None, None, None)) >> dispatch)
                disableChangeLevel
        ]
        Bulma.column [ model.dicePool |> dicePoolToString |> prop.text ]
    ]