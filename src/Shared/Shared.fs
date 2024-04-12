namespace Shared

// open FogentRoleplayLib.Item
// open FogentRoleplayLib.MagicSkill
// open FogentRoleplayLib.MagicCombat
// open FogentRoleplayLib.Range
// open FogentRoleplayLib.EffectForDisplay
// open FogentRoleplayLib.CarryWeightCalculation
// open FogentRoleplayLib.MovementSpeedEffect
// open FogentRoleplayLib.WeightClass
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.CoreSkill
// open FogentRoleplayLib.ItemStack

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type FogentRoleplayData = {
    defaultAttributeSet: AttributeName Set
    defaultCoreSkillList: CoreSkill List
//   allItemStackList: ItemStack list
//   magicSkillMap: Map<string, MagicSkill>
//   magicCombatMap: Map<string, MagicCombat>
//   rangeMap: Map<string, Range>
//   combatVocationalSkill: string list
//   effectForDisplayMap: Map<string, EffectForDisplay>
//   carryWeightCalculationMap: Map<string, CarryWeightCalculation>
//   weightClassList: WeightClass List
//   movementSpeedCalculationMap: Map<string, MovementSpeedCalculation>
}

type IFogentRoleplayDataApi = {
    getInitData: unit -> Async<FogentRoleplayData>
}