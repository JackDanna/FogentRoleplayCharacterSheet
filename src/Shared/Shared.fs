namespace Shared

open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.SkillName
open FogentRoleplayLib.ItemStack
open FogentRoleplayLib.WeaponSpell
open FogentRoleplayLib.MagicSystem
open FogentRoleplayLib.WeaponSkillData
open FogentRoleplayLib.Effect
open FogentRoleplayLib.CombatSpeedCalculation
open FogentRoleplayLib.CoreSkillData

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type FogentRoleplayData = {
    attributeNameSet: AttributeName Set
    coreSkillDataSet: CoreSkillData Set
    itemStackMap: Map<string, ItemStack>
    weaponSpellSet: WeaponSpell Set
    magicSystemMap: Map<string, MagicSystem>
    weaponSkillData: WeaponSkillData Set
    effectMap: Map<string, Effect>
    //   carryWeightCalculationMap: Map<string, CarryWeightCalculation>
    //   weightClassList: WeightClass List
    combatSpeedCalculationMap: Map<string, CombatSpeedCalculation>
}

type IFogentRoleplayDataApi = {
    getInitData: unit -> Async<FogentRoleplayData>
}