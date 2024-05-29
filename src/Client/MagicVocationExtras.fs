module MagicVocationExtras

open FogentRoleplayLib.MagicVocationExtras
open FogentRoleplayLib.MagicResourcePool
open FogentRoleplayLib.Skill
open FogentRoleplayLib.Neg1To5
open FogentRoleplayLib.MagicSystem
open FogentRoleplayLib.DicePool
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | MagicVocationSkillsMsg of MagicVocationSkills.Msg
    | SetCurrentMagicResource of uint
    | CalculateMagicVocationSkillDicePools of DicePoolCalculationData

let init
    (coreSkillMap: Map<string, Skill>)
    (magicSystem: MagicSystem)
    vocationStatLevel
    vocationStatDicePool
    : MagicVocationExtras =

    let governingCoreSkillLevel, governingCoreSkillDicePoolSize =
        match coreSkillMap.TryFind magicSystem.governingCoreSkill with
        | None -> (Zero, 0u)
        | Some coreSkill -> (coreSkill.level, dicePoolToNumDice coreSkill.dicePool)

    let magicResourceCap =
        calculateMagicResourcePool
            vocationStatLevel
            (dicePoolToNumDice vocationStatDicePool)
            governingCoreSkillLevel
            governingCoreSkillDicePoolSize

    {
        magicVocationSkills = MagicVocationSkills.init ()
        currentMagicResource = magicResourceCap
        magicResourceCap = magicResourceCap
        magicSystem = magicSystem
    }

let update msg (model: MagicVocationExtras) =
    match msg with
    | MagicVocationSkillsMsg msg ->
        let temp msg =
            match msg with
            | MagicVocationSkills.InsertMagicVocationSkill(name,
                                                           _,
                                                           dicePoolCalculationDataOption,
                                                           weaponSkillDataOption,
                                                           _) ->
                MagicVocationSkills.InsertMagicVocationSkill(
                    name,
                    Some model.magicSystem.vocationGoverningAttributeSet,
                    dicePoolCalculationDataOption,
                    weaponSkillDataOption,
                    Some model.magicSystem.magicSkillDataMap
                )
            | _ -> msg

        {
            model with
                magicVocationSkills = MagicVocationSkills.update (temp msg) model.magicVocationSkills
        }

    | SetCurrentMagicResource newCurrentMagicResource -> {
        model with
            currentMagicResource =
                if newCurrentMagicResource > model.magicResourceCap then
                    model.magicResourceCap
                else
                    newCurrentMagicResource
      }
    | CalculateMagicVocationSkillDicePools dicePoolCalculationData -> {
        model with
            magicVocationSkills =
                MagicVocationSkills.update
                    (MagicVocationSkills.CalculateDicePools(dicePoolCalculationData))
                    model.magicVocationSkills
      }

open Feliz
open Feliz.Bulma

let view attributeNameSet (weaponSkillNames) (model: MagicVocationExtras) dispatch =

    MagicVocationSkills.view
        attributeNameSet
        weaponSkillNames
        model.magicVocationSkills
        (MagicVocationSkillsMsg >> dispatch)
    @ [
        Bulma.columns [
            Bulma.column [ prop.text model.magicSystem.resourceName ]
            Bulma.column [
                Bulma.input.number [
                    prop.min 0
                    prop.value (int model.currentMagicResource)
                    prop.onChange (fun (num: int) -> dispatch (SetCurrentMagicResource(uint num)))
                ]
            ]
            Bulma.column [ sprintf "Max: %d" model.magicResourceCap |> prop.text ]
        ]
    ]