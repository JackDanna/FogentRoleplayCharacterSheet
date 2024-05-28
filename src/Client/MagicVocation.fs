module MagicVocation

open FogentRoleplayLib.MagicVocation
open FogentRoleplayLib.MagicResourcePool
open FogentRoleplayLib.Skill
open FogentRoleplayLib.Neg1To5
open FogentRoleplayLib.MagicSystem
open FogentRoleplayLib.DicePool
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | VocationStatMsg of VocationStat.Msg
    | MagicVocationSkillsMsg of MagicVocationSkills.Msg
    | SetCurrentMagicResource of uint
    | CalculateMagicVocationSkillDicePools of DicePoolCalculationData

let init name (coreSkillMap: Map<string, Skill>) (magicSystem: MagicSystem) dicePoolCalculationData : MagicVocation =
    let vocationStat = VocationStat.init name dicePoolCalculationData

    let governingCoreSkillLevel, governingCoreSkillDicePoolSize =
        match coreSkillMap.TryFind magicSystem.governingCoreSkill with
        | None -> (Zero, 0u)
        | Some coreSkill -> (coreSkill.level, dicePoolToNumDice coreSkill.dicePool)

    let magicResourceCap =
        calculateMagicResourcePool
            vocationStat.level
            (dicePoolToNumDice vocationStat.dicePool)
            governingCoreSkillLevel
            governingCoreSkillDicePoolSize

    {
        vocationStat = vocationStat
        magicVocationSkills = MagicVocationSkills.init ()
        currentMagicResource = magicResourceCap
        magicResourceCap = magicResourceCap
        magicSystem = magicSystem
    }

let update msg (model: MagicVocation) =
    match msg with
    | VocationStatMsg msg -> {
        model with
            vocationStat = VocationStat.update msg model.vocationStat
      }
    | MagicVocationSkillsMsg msg ->
        let temp msg =
            match msg with
            | MagicVocationSkills.InsertMagicVocationSkill(name,
                                                           _,
                                                           dicePoolCalculationDataOption,
                                                           weaponSkillDataOption,
                                                           magicSkillDataMapOption) ->
                MagicVocationSkills.InsertMagicVocationSkill(
                    name,
                    Some model.vocationStat.governingAttributeNameSet,
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
            vocationStat =
                VocationStat.update (VocationStat.CalculateDicePool(dicePoolCalculationData)) model.vocationStat
            magicVocationSkills =
                MagicVocationSkills.update
                    (MagicVocationSkills.CalculateDicePools(dicePoolCalculationData))
                    model.magicVocationSkills
      }

open Feliz
open Feliz.Bulma

let view attributeNameSet (weaponSkillNames) (model: MagicVocation) dispatch =


    [
        VocationStat.view attributeNameSet model.vocationStat (VocationStatMsg >> dispatch)
    ]
    @ MagicVocationSkills.view
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