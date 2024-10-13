module LiteDBDatabase

open LiteDB
open LiteDB.FSharp
open Shared

open FogentRoleplayLib.Character

[<AutoOpenAttribute>]
module LiteDBTypes =
    open FogentRoleplayLib
    open Attribute
    open Skill
    open ZeroToThree
    open Vocation
    open DicePoolCalculation
    open ItemElement
    open CombatRoll
    open CharacterInformation
    open Effect
    open CombatSpeed
    open SettingData
    open WeightClass
    open CarryWeightCalculation
    open CoreSkillData
    open ListUtils
    open WeightClass
    open SkillName
    open Neg1To5
    open AttributeName
    open DicePoolMod
    open ZeroToFive
    open DicePool
    open MundaneOrMagicVocationExtras
    open DamageType
    open Range
    open AreaOfEffect
    open ResourceName
    open Penetration
    open EngageableOpponents
    open DurationAndSource
    open CalculatedRange
    open SetAreaOfEffect
    open WeaponSpell
    open CombatSpeedCalculation
    open WeaponResource
    open Weapon
    open AttributeDeterminedDiceMod
    open FogentRoleplayLib.Item
    open FogentRoleplayLib.WeaponSkillData
    open FogentRoleplayLib.VocationStat
    open FogentRoleplayLib.MundaneVocationSkill
    open FogentRoleplayLib.MagicSkillData
    open FogentRoleplayLib.MagicSystem
    open FogentRoleplayLib.MagicVocationExtras
    open FogentRoleplayLib.SkillDiceMod
    open FogentRoleplayLib.AttributeStatAdjustment
    open FogentRoleplayLib.PhysicalDefense
    open FogentRoleplayLib.BaseDiceMod
    open FogentRoleplayLib.TextEffect
    open FogentRoleplayLib.ItemStack
    open FogentRoleplayLib.Container
    open FogentRoleplayLib.BaseDiceTier

    type LiteDB_WeaponResource = {
        name: string
        resourceName: ResourceName
        dicePoolMod: DicePoolMod
        penetration: Penetration
        rangeOption: Range option
        damageTypeSet: DamageType seq
        namedAreaOfEffectOption: AreaOfEffect option
    }

    let toWeaponResource x : WeaponResource = {
        name = x.name
        resourceName = x.resourceName
        dicePoolMod = x.dicePoolMod
        penetration = x.penetration
        rangeOption = x.rangeOption
        damageTypeSet = Set.ofSeq x.damageTypeSet
        namedAreaOfEffectOption = x.namedAreaOfEffectOption
    }

    let toLiteDB_WeaponResource (x: WeaponResource) = {
        name = x.name
        resourceName = x.resourceName
        dicePoolMod = x.dicePoolMod
        penetration = x.penetration
        rangeOption = x.rangeOption
        damageTypeSet = x.damageTypeSet
        namedAreaOfEffectOption = x.namedAreaOfEffectOption
    }

    type LiteDB_Weapon = {
        name: string
        governingSkillName: SkillName
        oneHandedDiceMod: DicePoolMod option
        twoHandedDiceMod: DicePoolMod option
        penetration: Penetration
        range: Range
        damageTypes: DamageType seq
        engageableOpponents: EngageableOpponents
        dualWieldedDiceMod: DicePoolMod option
        areaOfEffectOption: AreaOfEffect option
        resourceNameOption: ResourceName option
        baseDiceTier: BaseDiceTier
    }

    let toWeapon (x: LiteDB_Weapon) : Weapon = {
        name = x.name
        governingSkillName = x.governingSkillName
        oneHandedDiceMod = x.oneHandedDiceMod
        twoHandedDiceMod = x.twoHandedDiceMod
        penetration = x.penetration
        range = x.range
        damageTypes = Set.ofSeq x.damageTypes
        engageableOpponents = x.engageableOpponents
        dualWieldedDiceMod = x.dualWieldedDiceMod
        areaOfEffectOption = x.areaOfEffectOption
        resourceNameOption = x.resourceNameOption
        baseDiceTier = x.baseDiceTier
    }

    let toLiteDB_Weapon (x: Weapon) : LiteDB_Weapon = {
        name = x.name
        governingSkillName = x.governingSkillName
        oneHandedDiceMod = x.oneHandedDiceMod
        twoHandedDiceMod = x.twoHandedDiceMod
        penetration = x.penetration
        range = x.range
        damageTypes = x.damageTypes
        engageableOpponents = x.engageableOpponents
        dualWieldedDiceMod = x.dualWieldedDiceMod
        areaOfEffectOption = x.areaOfEffectOption
        resourceNameOption = x.resourceNameOption
        baseDiceTier = x.baseDiceTier
    }

    type LiteDB_AttributeDeterminedDiceMod = {
        name: string
        attributesToEffect: AttributeName seq
        dicePoolMod: DicePoolMod
        durationAndSource: DurationAndSource
    }

    let toAttributeDeterminedDiceMod x : AttributeDeterminedDiceMod = {
        name = x.name
        attributesToEffect = Set.ofSeq x.attributesToEffect
        dicePoolMod = x.dicePoolMod
        durationAndSource = x.durationAndSource
    }

    let toLiteDB_AttributeDeterminedDiceMod (x: AttributeDeterminedDiceMod) : LiteDB_AttributeDeterminedDiceMod = {
        name = x.name
        attributesToEffect = x.attributesToEffect
        dicePoolMod = x.dicePoolMod
        durationAndSource = x.durationAndSource
    }

    type LiteDB_Effect =
        | Weapon of LiteDB_Weapon
        | WeaponResource of LiteDB_WeaponResource
        | SkillDiceMod of SkillDiceMod
        | AttributeStatAdjustment of AttributeStatAdjustment
        | PhysicalDefense of PhysicalDefense
        | AttributeDeterminedDiceMod of LiteDB_AttributeDeterminedDiceMod
        | BaseDiceMod of BaseDiceMod
        | TextEffect of TextEffect

    let toEffect =
        function
        | Weapon weapon -> toWeapon weapon |> Effect.Weapon
        | WeaponResource weaponResource -> toWeaponResource weaponResource |> Effect.WeaponResource
        | SkillDiceMod x -> Effect.SkillDiceMod x
        | AttributeStatAdjustment x -> Effect.AttributeStatAdjustment x
        | PhysicalDefense x -> Effect.PhysicalDefense x
        | AttributeDeterminedDiceMod x -> toAttributeDeterminedDiceMod x |> Effect.AttributeDeterminedDiceMod
        | BaseDiceMod x -> Effect.BaseDiceMod x
        | TextEffect x -> Effect.TextEffect x

    let toLiteDB_Effect =
        function
        | Effect.Weapon weapon -> toLiteDB_Weapon weapon |> Weapon
        | Effect.WeaponResource weaponResource -> toLiteDB_WeaponResource weaponResource |> WeaponResource
        | Effect.SkillDiceMod x -> SkillDiceMod x
        | Effect.AttributeStatAdjustment x -> AttributeStatAdjustment x
        | Effect.PhysicalDefense x -> PhysicalDefense x
        | Effect.AttributeDeterminedDiceMod x -> toLiteDB_AttributeDeterminedDiceMod x |> AttributeDeterminedDiceMod
        | Effect.BaseDiceMod x -> BaseDiceMod x
        | Effect.TextEffect x -> TextEffect x

    type LiteDB_Item = {
        name: string
        itemEffectSet: LiteDB_Effect seq
        value: string
        weight: float
    }

    let toItem x : Item = {
        name = x.name
        itemEffectSet = x.itemEffectSet |> Seq.map toEffect |> Set.ofSeq
        value = x.value
        weight = x.weight
    }

    let toLiteDB_Item (x: Item) = {
        name = x.name
        itemEffectSet = x.itemEffectSet |> Seq.map toLiteDB_Effect
        value = x.value
        weight = x.weight
    }

    type LiteDB_ItemStack = { item: LiteDB_Item; quantity: uint }

    let toItemStack x : ItemStack = {
        item = toItem x.item
        quantity = x.quantity
    }

    let toLiteDB_ItemStack (x: ItemStack) = {
        item = toLiteDB_Item x.item
        quantity = x.quantity
    }

    type LiteDB_ContainerItem = {
        item: LiteDB_Item
        containerTypeData: Container
        containedElements: LiteDB_ItemElement list
    }

    and LiteDB_ItemElement =
        | Item of LiteDB_Item
        | ContainerItem of LiteDB_ContainerItem
        | ItemStack of LiteDB_ItemStack

    let rec toContainerItem x : ContainerItem = {
        item = toItem x.item
        containerTypeData = x.containerTypeData
        containedElements = x.containedElements |> List.map toItemElement
    }

    and toItemElement =
        function
        | Item item -> toItem item |> ItemElement.Item
        | ContainerItem x -> toContainerItem x |> ItemElement.ContainerItem
        | ItemStack x -> toItemStack x |> ItemElement.ItemStack

    let rec toLiteDB_ContainerItem (x: ContainerItem) = {
        item = toLiteDB_Item x.item
        containerTypeData = x.containerTypeData
        containedElements = x.containedElements |> List.map toLiteDB_ItemElement
    }

    and toLiteDB_ItemElement =
        function
        | ItemElement.Item item -> toLiteDB_Item item |> Item
        | ItemElement.ContainerItem x -> toLiteDB_ContainerItem x |> ContainerItem
        | ItemElement.ItemStack x -> toLiteDB_ItemStack x |> ItemStack

    type LiteDB_Skill = {
        name: SkillName
        level: Neg1To5
        governingAttributeNames: AttributeName seq
        dicePool: DicePool
    }

    let toSkill x : Skill = {
        name = x.name
        level = x.level
        governingAttributeNames = Set.ofSeq x.governingAttributeNames
        dicePool = x.dicePool
    }

    let toLiteDB_Skill (x: Skill) = {
        name = x.name
        level = x.level
        governingAttributeNames = x.governingAttributeNames
        dicePool = x.dicePool
    }

    type LiteDB_WeaponSkillData = {
        name: string
        governingAttributes: AttributeName seq
    }

    let toWeaponSkillData x : WeaponSkillData = {
        name = x.name
        governingAttributes = Set.ofSeq x.governingAttributes
    }

    let toLiteDB_WeaponSkillData (x: WeaponSkillData) = {
        name = x.name
        governingAttributes = x.governingAttributes
    }

    type LiteDB_VocationStat = {
        name: string
        governingAttributeNameSet: AttributeName seq
        level: ZeroToFive
        dicePool: DicePool
    }

    let toVocationStat x : VocationStat = {
        name = x.name
        governingAttributeNameSet = Set.ofSeq x.governingAttributeNameSet
        level = x.level
        dicePool = x.dicePool
    }

    let toLiteDB_VocationStat (x: VocationStat) = {
        name = x.name
        governingAttributeNameSet = x.governingAttributeNameSet
        level = x.level
        dicePool = x.dicePool
    }

    type LiteDB_MundaneVocationSkill =
        | VocationalSkill of LiteDB_Skill
        | WeaponSkill of LiteDB_Skill

    let toMundaneVocationSkill =
        function
        | VocationalSkill skill -> toSkill skill |> MundaneVocationSkill.VocationalSkill
        | WeaponSkill skill -> toSkill skill |> MundaneVocationSkill.WeaponSkill

    let toLiteDB_MundaneVocationSkill =
        function
        | MundaneVocationSkill.VocationalSkill skill -> toLiteDB_Skill skill |> VocationalSkill
        | MundaneVocationSkill.WeaponSkill skill -> toLiteDB_Skill skill |> WeaponSkill

    type LiteDB_MagicSkillData = {
        name: SkillName
        damageTypes: DamageType seq
        isMeleeCapable: bool
        isRangeCapable: bool
    }

    let toMagicSkillData x : MagicSkillData = {
        name = x.name
        damageTypes = x.damageTypes |> Set.ofSeq
        isMeleeCapable = x.isMeleeCapable
        isRangeCapable = x.isRangeCapable
    }

    let toLiteDB_MagicSkillData (x: MagicSkillData) = {
        name = x.name
        damageTypes = x.damageTypes
        isMeleeCapable = x.isMeleeCapable
        isRangeCapable = x.isRangeCapable
    }

    type LiteDB_MagicSystem = {
        name: string
        vocationName: string
        vocationGoverningAttributeSet: AttributeName seq
        resourceName: string
        governingCoreSkill: string
        magicSkillDataSet: LiteDB_MagicSkillData seq
    }

    let toMagicSystem x : MagicSystem = {
        name = x.name
        vocationName = x.vocationName
        vocationGoverningAttributeSet = Set.ofSeq x.vocationGoverningAttributeSet
        resourceName = x.resourceName
        governingCoreSkill = x.governingCoreSkill
        magicSkillDataSet = x.magicSkillDataSet |> Seq.map toMagicSkillData |> Set.ofSeq
    }

    let toLiteDB_MagicSystem (x: MagicSystem) = {
        name = x.name
        vocationName = x.vocationName
        vocationGoverningAttributeSet = x.vocationGoverningAttributeSet
        resourceName = x.resourceName
        governingCoreSkill = x.governingCoreSkill
        magicSkillDataSet = x.magicSkillDataSet |> Seq.map toLiteDB_MagicSkillData
    }

    type LiteDB_MagicVocationSkill =
        | MagicSkill of LiteDB_Skill
        | MundaneVocationSkill of LiteDB_MundaneVocationSkill

    let toMagicVocationSkill =
        function
        | MagicSkill skill -> toSkill skill |> MagicVocationSkill.MagicSkill
        | MundaneVocationSkill skill -> toMundaneVocationSkill skill |> MagicVocationSkill.MundaneVocationSkill

    let toLiteDB_MagicVocationSkill =
        function
        | MagicVocationSkill.MagicSkill skill -> toLiteDB_Skill skill |> MagicSkill
        | MagicVocationSkill.MundaneVocationSkill skill -> toLiteDB_MundaneVocationSkill skill |> MundaneVocationSkill

    type LiteDB_MagicVocationExtras = {
        magicVocationSkills: LiteDB_MagicVocationSkill seq
        magicSystem: LiteDB_MagicSystem
        vocationResourcePool: uint
        coreSkillResourcePool: uint
        currentMagicResource: uint
    }

    let toMagicVocationExtras x : MagicVocationExtras = {
        magicVocationSkills = x.magicVocationSkills |> Seq.map toMagicVocationSkill |> Set.ofSeq
        magicSystem = toMagicSystem x.magicSystem
        vocationResourcePool = x.vocationResourcePool
        coreSkillResourcePool = x.coreSkillResourcePool
        currentMagicResource = x.currentMagicResource
    }

    let toLiteDB_MagicVocationExtras (x: MagicVocationExtras) = {
        magicVocationSkills = x.magicVocationSkills |> Seq.map toLiteDB_MagicVocationSkill
        magicSystem = toLiteDB_MagicSystem x.magicSystem
        vocationResourcePool = x.vocationResourcePool
        coreSkillResourcePool = x.coreSkillResourcePool
        currentMagicResource = x.currentMagicResource
    }

    type LiteDB_MundaneOrMagicVocationExtras =
        | MundaneVocationExtras of LiteDB_MundaneVocationSkill seq
        | MagicVocationExtras of LiteDB_MagicVocationExtras

    let toMundaneOrmagicVocationExtras =
        function
        | MundaneVocationExtras skills ->
            skills
            |> Seq.map toMundaneVocationSkill
            |> Set.ofSeq
            |> MundaneOrMagicVocationExtras.MundaneVocationExtras
        | MagicVocationExtras magicVocationExtras ->
            magicVocationExtras
            |> toMagicVocationExtras
            |> MundaneOrMagicVocationExtras.MagicVocationExtras

    let toLiteDB_MundaneOrmagicVocationExtras =
        function
        | MundaneOrMagicVocationExtras.MundaneVocationExtras skills ->
            skills |> Seq.map toLiteDB_MundaneVocationSkill |> MundaneVocationExtras
        | MundaneOrMagicVocationExtras.MagicVocationExtras magicVocationExtras ->
            magicVocationExtras |> toLiteDB_MagicVocationExtras |> MagicVocationExtras

    type LiteDB_Vocation = {
        vocationStat: LiteDB_VocationStat
        mundaneOrMagicVocationExtras: LiteDB_MundaneOrMagicVocationExtras
    }

    let toVocation x : Vocation = {
        vocationStat = toVocationStat x.vocationStat
        mundaneOrMagicVocationExtras = toMundaneOrmagicVocationExtras x.mundaneOrMagicVocationExtras
    }

    let toLiteDB_Vocation (x: Vocation) = {
        vocationStat = toLiteDB_VocationStat x.vocationStat
        mundaneOrMagicVocationExtras = toLiteDB_MundaneOrmagicVocationExtras x.mundaneOrMagicVocationExtras
    }

    type LiteDB_CombatRoll = {
        itemName: string
        weaponTypeName: string
        handedVariation: string
        resourceName: string
        resourceDicePoolMod: DicePoolMod
        dicePool: DicePool
        weaponandOffhandDicePoolModString: DicePoolMod
        calculatedRange: CalculatedRange
        penetration: Penetration
        damageTypeSet: DamageType seq
        setAreaOfEffectOption: SetAreaOfEffect Option
        calculatedEngageableOpponents: CalculatedEngageableOpponents
        eoName: string option
    }

    let toCombatRoll x : CombatRoll = {
        itemName = x.itemName
        weaponTypeName = x.weaponTypeName
        handedVariation = x.handedVariation
        resourceName = x.resourceName
        resourceDicePoolMod = x.resourceDicePoolMod
        dicePool = x.dicePool
        weaponandOffhandDicePoolModString = x.weaponandOffhandDicePoolModString
        calculatedRange = x.calculatedRange
        penetration = x.penetration
        damageTypeSet = x.damageTypeSet |> Set.ofSeq
        setAreaOfEffectOption = x.setAreaOfEffectOption
        calculatedEngageableOpponents = x.calculatedEngageableOpponents
        eoName = x.eoName
    }

    let toLiteDB_CombatRoll (x: CombatRoll) = {
        itemName = x.itemName
        weaponTypeName = x.weaponTypeName
        handedVariation = x.handedVariation
        resourceName = x.resourceName
        resourceDicePoolMod = x.resourceDicePoolMod
        dicePool = x.dicePool
        weaponandOffhandDicePoolModString = x.weaponandOffhandDicePoolModString
        calculatedRange = x.calculatedRange
        penetration = x.penetration
        damageTypeSet = x.damageTypeSet
        setAreaOfEffectOption = x.setAreaOfEffectOption
        calculatedEngageableOpponents = x.calculatedEngageableOpponents
        eoName = x.eoName
    }

    type LiteDB_WeightClass = {
        name: string
        bottomPercentOption: float option
        topPercentOption: float option
        attributeDeterminedDiceModEffect: LiteDB_AttributeDeterminedDiceMod
    }

    let toWeightClass x : WeightClass = {
        name = x.name
        bottomPercentOption = x.bottomPercentOption
        topPercentOption = x.bottomPercentOption
        attributeDeterminedDiceModEffect = toAttributeDeterminedDiceMod x.attributeDeterminedDiceModEffect
    }

    let toLiteDB_WeightClass (x: WeightClass) = {
        name = x.name
        bottomPercentOption = x.bottomPercentOption
        topPercentOption = x.bottomPercentOption
        attributeDeterminedDiceModEffect = toLiteDB_AttributeDeterminedDiceMod x.attributeDeterminedDiceModEffect
    }

    type LiteDB_SettingData = {
        attributeNameSet: AttributeName seq
        coreSkillDataSet: CoreSkillData seq
        itemElementSet: LiteDB_ItemElement seq
        weaponSpellSet: WeaponSpell seq
        magicSystemSet: LiteDB_MagicSystem seq
        weaponSkillDataSet: LiteDB_WeaponSkillData seq
        effectSet: LiteDB_Effect seq
        combatSpeedCalculationSet: CombatSpeedCalculation seq
        carryWeightCalculationSet: CarryWeightCalculation seq
        weightClassSet: LiteDB_WeightClass seq
    }

    let toSettingData x : SettingData = {
        attributeNameSet = Set.ofSeq x.attributeNameSet
        coreSkillDataSet = Set.ofSeq x.coreSkillDataSet
        itemElementSet = x.itemElementSet |> Seq.map toItemElement |> Set.ofSeq
        weaponSpellSet = Set.ofSeq x.weaponSpellSet
        magicSystemSet = x.magicSystemSet |> Seq.map toMagicSystem |> Set.ofSeq
        weaponSkillDataSet = x.weaponSkillDataSet |> Seq.map toWeaponSkillData |> Set.ofSeq
        effectSet = x.effectSet |> Seq.map toEffect |> Set.ofSeq
        combatSpeedCalculationSet = Set.ofSeq x.combatSpeedCalculationSet
        carryWeightCalculationSet = Set.ofSeq x.carryWeightCalculationSet
        weightClassSet = x.weightClassSet |> Seq.map toWeightClass |> Set.ofSeq
    }

    let toLiteDB_SettingData (x: SettingData) = {
        attributeNameSet = x.attributeNameSet
        coreSkillDataSet = x.coreSkillDataSet
        itemElementSet = x.itemElementSet |> Seq.map toLiteDB_ItemElement
        weaponSpellSet = x.weaponSpellSet
        magicSystemSet = x.magicSystemSet |> Seq.map toLiteDB_MagicSystem
        weaponSkillDataSet = x.weaponSkillDataSet |> Seq.map toLiteDB_WeaponSkillData
        effectSet = x.effectSet |> Seq.map toLiteDB_Effect
        combatSpeedCalculationSet = x.combatSpeedCalculationSet
        carryWeightCalculationSet = x.carryWeightCalculationSet
        weightClassSet = x.weightClassSet |> Seq.map toLiteDB_WeightClass
    }

    type LiteDB_Character = {
        id: int
        name: string
        attributes: Attribute seq
        coreSkills: LiteDB_Skill seq
        destinyPoints: ZeroToThree
        vocationList: LiteDB_Vocation list
        equipment: LiteDB_ItemElement List
        combatRollList: LiteDB_CombatRoll List
        characterInformation: CharacterInformation
        characterEffects: LiteDB_Effect List
        combatSpeeds: CombatSpeed List
        settingData: LiteDB_SettingData
        weightClassOption: LiteDB_WeightClass option
        carryWeightCalculationOption: CarryWeightCalculation option
    }

    let toCharacter x : Character = {
        id = x.id
        name = x.name
        attributes = Set.ofSeq x.attributes
        coreSkills = x.coreSkills |> Seq.map toSkill |> Set.ofSeq
        destinyPoints = x.destinyPoints
        vocationList = x.vocationList |> List.map toVocation
        equipment = x.equipment |> List.map toItemElement
        combatRollList = x.combatRollList |> List.map toCombatRoll
        characterInformation = x.characterInformation
        characterEffects = x.characterEffects |> List.map toEffect
        combatSpeeds = x.combatSpeeds
        settingData = toSettingData x.settingData
        weightClassOption =
            match x.weightClassOption with
            | None -> None
            | Some weightClass -> weightClass |> toWeightClass |> Some
        carryWeightCalculationOption = x.carryWeightCalculationOption
    }

    let toLiteDB_Character (x: Character) = {
        id = x.id
        name = x.name
        attributes = x.attributes
        coreSkills = x.coreSkills |> Seq.map toLiteDB_Skill
        destinyPoints = x.destinyPoints
        vocationList = x.vocationList |> List.map toLiteDB_Vocation
        equipment = x.equipment |> List.map toLiteDB_ItemElement
        combatRollList = x.combatRollList |> List.map toLiteDB_CombatRoll
        characterInformation = x.characterInformation
        characterEffects = x.characterEffects |> List.map toLiteDB_Effect
        combatSpeeds = x.combatSpeeds
        settingData = toLiteDB_SettingData x.settingData
        weightClassOption =
            match x.weightClassOption with
            | None -> None
            | Some weightClass -> weightClass |> toLiteDB_WeightClass |> Some
        carryWeightCalculationOption = x.carryWeightCalculationOption
    }

    [<CLIMutableAttribute>]
    type LiteDB_IdCharacter = {
        Id: int
        LiteDB_Character: LiteDB_Character
    }

    let toIdCharacter (x: LiteDB_IdCharacter) : IdCharacter = {
        Id = x.Id
        Character = toCharacter x.LiteDB_Character
    }

    let toLiteDB_IdCharacter (x: IdCharacter) : LiteDB_IdCharacter = {
        Id = x.Id
        LiteDB_Character = toLiteDB_Character x.Character
    }

let db =
    let mapper = FSharpBsonMapper()
    let connStr = "Filename=fogentData.db;mode=Exclusive"
    new LiteDatabase(connStr, mapper)

let collectionFromDB<'T> = db.GetCollection<'T>(typeof<'T>.Name)

// let insertEntity (entity: 'T) =
//     // Since Id is set to 0, inserted entities will be placed according to auto-incrementation
//     let idEntity = { Id = 0; Entity = entity }
//     collectionFromDB.Insert(idEntity) |> ignore
//     idEntity

// let findEntity entity = collectionFromDB.FindById entity.Id

// let insertEntities entities =
//     entities |> Seq.map (fun entity -> insertEntity entity)

[<CLIMutable>]
type UserCharacterAccess = {
    Id: int
    UserId: int
    CharacterId: int
//AccessGranted: System.DateTime
//AccessType: string
}

let users = collectionFromDB<IdUser>
let liteDB_IdCharacters = collectionFromDB<LiteDB_IdCharacter>
let userCharacterAccesses = collectionFromDB<UserCharacterAccess>

//users.EnsureIndex(fun (u: IdUser) -> u.Entity.userName) |> ignore

let insertNewUser (user: Login) =
    let idEntity = { Id = 0; Login = user }
    collectionFromDB.Insert(idEntity) |> ignore
    idEntity

let grantAccess userId characterId =
    userCharacterAccesses.Insert(
        {
            Id = 0
            UserId = userId
            CharacterId = characterId
        //AccessGrantedDate = System.DateTime.UtcNow
        //AccessType = accessType
        }
    )
    |> ignore

let insertNewCharacter userId (character: Character) =
    let idCharacter: LiteDB_IdCharacter =
        character |> createAutoIncrementedIdCharacter |> toLiteDB_IdCharacter

    liteDB_IdCharacters.Insert(idCharacter) |> ignore
    grantAccess userId idCharacter.Id
    idCharacter

let userIdToOwnedIdCharacters userId =
    userCharacterAccesses.Find(fun uca -> uca.UserId = userId)
    |> Seq.map (fun uca -> liteDB_IdCharacters.FindOne(fun character -> character.Id = uca.CharacterId))
    |> Seq.map toIdCharacter

let getUsersForCharacter characterId =
    userCharacterAccesses.Find(fun uca -> uca.CharacterId = characterId)
    |> Seq.map (fun uca -> users.FindOne(fun user -> user.Id = uca.UserId))

let usernameToIdUser (username: Username) =
    users.Find(fun idUser -> idUser.Login.userName = username) |> Seq.tryHead

let addNewCharacter settingData username =

    match usernameToIdUser username with
    | Some idUser ->
        settingData
        |> FogentRoleplayLib.Character.init 0 // Setting ID to zero will tell the DB to autoincrement its list of known ids
        |> insertNewCharacter idUser.Id
        |> ignore

        userIdToOwnedIdCharacters idUser.Id
    | None -> Seq.empty
    |> List.ofSeq

let updateIdCharacter username (idCharacterToUpdate: IdCharacter) =
    username
    |> usernameToIdUser
    |> function
        | None -> ()
        | Some idUser ->
            let doesUserOwnCharacter =
                idUser.Id
                |> userIdToOwnedIdCharacters
                |> Seq.exists (fun ownedIdCharacter -> ownedIdCharacter.Id = idCharacterToUpdate.Id)

            if doesUserOwnCharacter then
                liteDB_IdCharacters.Update(toLiteDB_IdCharacter idCharacterToUpdate) |> ignore

let isValidUserLogin login =

    users.Find(
        Query.And(
            Query.EQ("Login.userName", BsonValue(login.userName)),
            Query.EQ("Login.password", BsonValue(login.password))
        )
    )
    |> Seq.tryHead
    |> (function
    | Some _ -> true
    | None ->
        insertNewUser login // TESTING, REMOVE ASAP: This automatically creates a user if it doesn't exists,
        false)