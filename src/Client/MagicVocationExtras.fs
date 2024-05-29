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
type RecalculateCoreSkillResourcePoolMsg = Map<string, Skill>

type Msg =
    | MagicVocationSkillsMsg of MagicVocationSkills.Msg
    | SetCurrentMagicResource of uint
    | CalculateMagicVocationSkillDicePools of DicePoolCalculationData
    | RecalculateVocationResourcePool of RecalculateVocationResourcePoolMsg
    | RecalculateCoreSkillResourePool of Map<string, Skill>

let tryFindCoreSkillInMapWithDefault (coreSkillMap: Map<string, Skill>) governingCoreSkillName =
    match coreSkillMap.TryFind governingCoreSkillName with
    | None -> (Neg1To5.Zero, emptyDicePool)
    | Some coreSkill -> (coreSkill.level, coreSkill.dicePool)

let checkForResourcePoolOverflow resourcePool currentMagicResource =
    if resourcePool < currentMagicResource then
        resourcePool
    else
        currentMagicResource

let init
    (coreSkillMap: Map<string, Skill>)
    (magicSystem: MagicSystem)
    vocationStatLevel
    vocationStatDicePool
    : MagicVocationExtras =

    let governingCoreSkillLevel, governingCoreSkillDicePool =
        tryFindCoreSkillInMapWithDefault coreSkillMap magicSystem.governingCoreSkill

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

        {
            model with
                vocationResourcePool = newVocationResourcePool
                currentMagicResource =
                    checkForResourcePoolOverflow
                        (newVocationResourcePool + model.coreSkillResourcePool)
                        model.currentMagicResource
        }

    | RecalculateCoreSkillResourePool coreSkillMap ->
        let governingCoreSkillLevel, governingCoreSkillDicePool =
            tryFindCoreSkillInMapWithDefault coreSkillMap model.magicSystem.governingCoreSkill

        let newCoreSkillResourcePool =
            calcGoverningSkillMagicResource governingCoreSkillLevel (dicePoolToNumDice governingCoreSkillDicePool)

        {
            model with
                coreSkillResourcePool = newCoreSkillResourcePool
                currentMagicResource =
                    checkForResourcePoolOverflow
                        (model.vocationResourcePool + newCoreSkillResourcePool)
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