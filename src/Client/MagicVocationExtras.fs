module MagicVocationExtras

open FogentRoleplayLib.MagicVocationExtras
open FogentRoleplayLib.MagicResourcePool
open FogentRoleplayLib.Skill
open FogentRoleplayLib.Neg1To5
open FogentRoleplayLib.MagicSystem
open FogentRoleplayLib.DicePool
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.ZeroToFive

type RecalculateVocationResourcePoolMsg = ZeroToFive * DicePool

type Msg =
    | MagicVocationSkillsMsg of MagicVocationSkills.Msg
    | SetCurrentMagicResource of uint
    | CalculateMagicVocationSkillDicePools of DicePoolCalculationData
    | RecalculateVocationResourcePool of RecalculateVocationResourcePoolMsg

let init
    (coreSkillMap: Map<string, Skill>)
    (magicSystem: MagicSystem)
    vocationStatLevel
    vocationStatDicePool
    : MagicVocationExtras =

    let governingCoreSkillLevel, governingCoreSkillDicePool =
        match coreSkillMap.TryFind magicSystem.governingCoreSkill with
        | None -> (Neg1To5.Zero, emptyDicePool)
        | Some coreSkill -> (coreSkill.level, coreSkill.dicePool)

    let vocationResourcePool =
        calculateVocationMagicResource vocationStatLevel (dicePoolToNumDice vocationStatDicePool)

    let governingCoreSkillResourcePool =
        calcGoverningSkillMagicResource governingCoreSkillLevel (dicePoolToNumDice governingCoreSkillDicePool)

    {
        magicVocationSkills = MagicVocationSkills.init ()
        currentMagicResource = vocationResourcePool + governingCoreSkillResourcePool
        vocationResourcePool = vocationResourcePool
        coreSkillResourcePool = governingCoreSkillResourcePool
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
                if newCurrentMagicResource > model.coreSkillResourcePool then
                    model.coreSkillResourcePool
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
    | RecalculateVocationResourcePool(vocationLevel, vocationDicePool) ->
        let newVocationResourcePool =
            calculateVocationMagicResource vocationLevel (dicePoolToNumDice vocationDicePool)

        let newResourcePool = (newVocationResourcePool + model.coreSkillResourcePool)

        {
            model with
                vocationResourcePool = newVocationResourcePool
                currentMagicResource =
                    if newResourcePool < model.currentMagicResource then
                        newResourcePool
                    else
                        model.currentMagicResource
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
            Bulma.column [
                sprintf "Max: %d" (model.vocationResourcePool + model.coreSkillResourcePool)
                |> prop.text
            ]
        ]
    ]