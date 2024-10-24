module MagicVocationExtras

open FogentRoleplayLib.MagicVocationExtras
open FogentRoleplayLib.MagicResourcePool
open FogentRoleplayLib.Skill
open FogentRoleplayLib.Neg1To5
open FogentRoleplayLib.MagicSystem
open FogentRoleplayLib.DicePool
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.ZeroToFive
open FogentRoleplayLib.MagicSkillData

type RecalculateVocationResourcePoolMsg = ZeroToFive * DicePool
type RecalculateCoreSkillResourcePoolMsg = Map<string, Skill>

type Msg =
    | MagicVocationSkillsMsg of MagicVocationSkills.Msg
    | SetCurrentMagicResource of uint
    | CalculateMagicVocationSkillDicePools of DicePoolCalculationData
    | RecalculateVocationResourcePool of RecalculateVocationResourcePoolMsg
    | RecalculateCoreSkillResourePool of Map<string, Skill>
    | CheckIfLevelCapExceededForSkills of Skill.ZeroToFiveAndDicePoolCalculationData
    | SetLevelForVocationalSkill of Skill.ZeroToFiveAndDicePoolCalculationData

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
                                                           vocationStatLevelOption,
                                                           _,
                                                           dicePoolCalculationDataOption,
                                                           weaponSkillDataOption,
                                                           _) ->
                MagicVocationSkills.InsertMagicVocationSkill(
                    name,
                    vocationStatLevelOption,
                    Some model.magicSystem.vocationGoverningAttributeSet,
                    dicePoolCalculationDataOption,
                    weaponSkillDataOption,
                    Some model.magicSystem.magicSkillDataSet
                )
            | _ -> msg

        {
            model with
                magicVocationSkills = MagicVocationSkills.update (temp msg) model.magicVocationSkills
        }

    | SetCurrentMagicResource newCurrentMagicResource ->
        let magicResourcePool = model.coreSkillResourcePool + model.vocationResourcePool

        {
            model with
                currentMagicResource =
                    if newCurrentMagicResource > magicResourcePool then
                        magicResourcePool
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
    | CheckIfLevelCapExceededForSkills data -> {
        model with
            magicVocationSkills =
                MagicVocationSkills.update
                    (MagicVocationSkills.CheckIfLevelCapExceededForSkills(data))
                    model.magicVocationSkills
      }
    | SetLevelForVocationalSkill data -> {
        model with
            magicVocationSkills =
                MagicVocationSkills.update
                    (MagicVocationSkills.Msg.SetLevelForVocationalSkills(data))
                    model.magicVocationSkills
      }

open Feliz
open ViewUtils

let magicResourceView (model: MagicVocationExtras) dispatch =
    horizontalDiv [
        prop.children [
            Html.text model.magicSystem.resourceName
            // Html.txt model.magicSystem.governingCoreSkill
            numberInput [
                prop.min 0
                prop.max (int (model.coreSkillResourcePool + model.vocationResourcePool))
                prop.value (int model.currentMagicResource)
                prop.onChange (fun (num: int) -> dispatch (SetCurrentMagicResource(uint num)))
            ]
            //Html.text $"CoreSkillResourcePool: {model.coreSkillResourcePool}"
            //Html.text $"VocationResourcePool: {model.vocationResourcePool}"
            sprintf "Max: %d" (model.vocationResourcePool + model.coreSkillResourcePool)
            |> Html.text
        ]
    ]

let view attributeNameSet (weaponSkillNames) (model: MagicVocationExtras) dispatch =

    let (tbody, insert) =
        MagicVocationSkills.view
            model.magicSystem.name
            attributeNameSet
            (Set.map (fun x -> x.name) model.magicSystem.magicSkillDataSet)
            weaponSkillNames
            model.magicVocationSkills
            (MagicVocationSkillsMsg >> dispatch)

    tbody, insert, (magicResourceView model dispatch)