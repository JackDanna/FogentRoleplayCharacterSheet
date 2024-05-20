module MagicVocation

open FogentRoleplayLib.MagicVocation
open FogentRoleplayLib.MagicResourcePool
open FogentRoleplayLib.Skill
open FogentRoleplayLib.Neg1To5
open FogentRoleplayLib.MagicSystem
open FogentRoleplayLib.DicePool
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | MundaneVocationMsg of MundaneVocation.Msg
    | MagicVocationSkillListMsg of MagicSkillList.Msg
    | SetCurrentMagicResource of uint
    | CalculateMagicVocationSkillDicePools of DicePoolCalculationData

let init name (coreSkillMap: Map<string, Skill>) (magicSystem: MagicSystem) dicePoolCalculationData : MagicVocation =
    let mundaneVocation = MundaneVocation.init name dicePoolCalculationData

    let governingCoreSkillLevel, governingCoreSkillDicePoolSize =
        match coreSkillMap.TryFind magicSystem.governingCoreSkill with
        | None -> (Zero, 0u)
        | Some coreSkill -> (coreSkill.level, dicePoolToNumDice coreSkill.dicePool)

    let magicResourceCap =
        calculateMagicResourcePool
            mundaneVocation.vocationStat.level
            (dicePoolToNumDice mundaneVocation.vocationStat.dicePool)
            governingCoreSkillLevel
            governingCoreSkillDicePoolSize

    {
        currentMagicResource = magicResourceCap
        magicResourceCap = magicResourceCap
        magicSystem = magicSystem
        mundaneVocation = mundaneVocation
        magicVocationSkillSet = Set.empty
    }

let update msg (model: MagicVocation) =
    match msg with
    | MundaneVocationMsg msg -> {
        model with
            mundaneVocation = MundaneVocation.update msg model.mundaneVocation
      }
    | MagicVocationSkillListMsg msg ->
        let temp msg =
            match msg with
            | MagicSkillList.InsertMagicSkill(name, _, dicePoolCalculationDataOption, magicSkillDataMapOption) ->
                MagicSkillList.InsertMagicSkill(
                    name,
                    Some model.mundaneVocation.vocationStat.governingAttributeNameSet,
                    dicePoolCalculationDataOption,
                    magicSkillDataMapOption
                )
            | _ -> msg

        {
            model with
                magicVocationSkillSet = MagicSkillList.update (temp msg) model.magicVocationSkillSet
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
            mundaneVocation =
                MundaneVocation.update
                    (MundaneVocation.CalculateDicePools(dicePoolCalculationData))
                    model.mundaneVocation
            magicVocationSkillSet =
                MagicSkillList.update
                    (MagicSkillList.CalculateMagicSkillDicePool(dicePoolCalculationData))
                    model.magicVocationSkillSet
      }

open Feliz
open Feliz.Bulma

let view attributeNameSet (weaponSkillNames) (model: MagicVocation) dispatch =
    [
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
    |> List.append (
        MundaneVocation.view attributeNameSet weaponSkillNames model.mundaneVocation (MundaneVocationMsg >> dispatch)
    )