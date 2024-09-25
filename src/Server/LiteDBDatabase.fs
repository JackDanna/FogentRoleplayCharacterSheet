module LiteDBDatabase

open System
open LiteDB
open LiteDB.FSharp
//open System.Collections.Generic
open Shared

open FogentRoleplayLib.Character

// [<CLIMutableAttribute>]
// type Mut_IdUser = Shared.IdUser

// [<CLIMutableAttribute>]
// type Mut_IdCharacter = Shared.IdCharacter


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
    open DicePoolMod
    open Range
    open DamageType
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

    type LiteDB_Item = {
        name: string
        itemEffectSet: Effect seq
        value: string
        weight: float
    }

    let toItem x : Item = {
        name = x.name
        itemEffectSet = Set.ofSeq x.itemEffectSet
        value = x.value
        weight = x.weight
    }

    let toLiteDB_Item (x: Item) = {
        name = x.name
        itemEffectSet = x.itemEffectSet
        value = x.value
        weight = x.weight
    }

    type LiteDB_DicePoolCalculationData = {
        attributes: Attribute seq
        effects: Effect List
    }

    let toDicePoolCalculationData x : DicePoolCalculationData = {
        attributes = Set.ofSeq x.attributes
        effects = x.effects
    }

    let toLiteDB_DicePoolCalculationData (x: DicePoolCalculationData) = {
        attributes = x.attributes
        effects = x.effects
    }

    type LiteDB_Skill = {
        name: SkillName
        level: Neg1To5
        governingAttributeNames: AttributeName seq
        dicePoolModList: DicePoolMod List
    }

    let toSkill x : Skill = {
        name = x.name
        level = x.level
        governingAttributeNames = Set.ofSeq x.governingAttributeNames
        dicePoolModList = x.dicePoolModList
    }

    let toLiteDB_Skill (x: Skill) = {
        name = x.name
        level = x.level
        governingAttributeNames = x.governingAttributeNames
        dicePoolModList = x.dicePoolModList
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

    type LiteDB_MundaneVocationSkill =
        | VocationalSkill of LiteDB_Skill
        | WeaponSkill of LiteDB_Skill

    type LiteDB_MundaneVocation = {
        vocationStat: LiteDB_VocationStat
        mundaneVocationSkills: LiteDB_MundaneVocationSkill seq
    }

    type LiteDB_MagicSkillData = {
        name: SkillName
        damageTypes: DamageType seq
        isMeleeCapable: bool
        isRangeCapable: bool
    }

    type LiteDB_MagicSystem = {
        name: string
        vocationName: string
        vocationGoverningAttributeSet: AttributeName seq
        resourceName: string
        governingCoreSkill: string
        magicSkillDataSet: LiteDB_MagicSkillData seq
    }

    type LiteDB_MagicVocationSkill =
        | MagicSkill of LiteDB_Skill
        | MundaneVocationSkill of LiteDB_MundaneVocationSkill

    type LiteDB_MagicVocationExtras = {
        magicVocationSkills: LiteDB_MagicVocationSkill seq
        magicSystem: LiteDB_MagicSystem
        vocationResourcePool: uint
        coreSkillResourcePool: uint
        currentMagicResource: uint
    }

    type LiteDB_MundaneOrMagicVocationExtras =
        | MundaneVocationExtras of LiteDB_MundaneVocationSkill seq
        | MagicVocationExtras of LiteDB_MagicVocationExtras

    type LiteDB_Vocation = {
        vocationStat: LiteDB_VocationStat
        mundaneOrMagicVocationExtras: LiteDB_MundaneOrMagicVocationExtras
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

    type LiteDB_SettingData = {
        attributeNameSet: AttributeName seq
        coreSkillDataSet: CoreSkillData seq
        itemElementSet: ItemElement seq
        weaponSpellSet: WeaponSpell seq
        magicSystemSet: LiteDB_MagicSystem seq
        weaponSkillDataSet: LiteDB_WeaponSkillData seq
        effectSet: Effect seq
        combatSpeedCalculationSet: CombatSpeedCalculation seq
        carryWeightCalculationSet: CarryWeightCalculation seq
        weightClassSet: WeightClass seq
    }

    type LiteDB_Character = {
        name: string
        attributes: Attribute List
        coreSkills: LiteDB_Skill List
        destinyPoints: ZeroToThree
        vocationList: LiteDB_Vocation list
        equipment: ItemElement List
        combatRollList: CombatRoll List
        characterInformation: CharacterInformation
        characterEffects: Effect List
        combatSpeeds: CombatSpeed List
        settingData: SettingData
        weightClassOption: WeightClass option
        carryWeightCalculationOption: CarryWeightCalculation option
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
let characters = collectionFromDB<IdCharacter>
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

let insertCharacter userId (character: Character) =
    let idCharacter = createAutoIncrementedIdCharacter character
    collectionFromDB.Insert(idCharacter) |> ignore
    grantAccess userId idCharacter.Id
    idCharacter

let getCharactersForUser userId =
    userCharacterAccesses.Find(fun uca -> uca.UserId = userId)
    |> Seq.map (fun uca -> characters.FindOne(fun character -> character.Id = uca.CharacterId))

let getUsersForCharacter characterId =
    userCharacterAccesses.Find(fun uca -> uca.CharacterId = characterId)
    |> Seq.map (fun uca -> users.FindOne(fun user -> user.Id = uca.UserId))

let usernameToIdUser (username: Username) =
    users.Find(fun idUser -> idUser.Login.userName = username) |> Seq.tryHead

let addNewCharacter settingData username =

    match usernameToIdUser username with
    | Some idUser ->
        settingData
        |> FogentRoleplayLib.Character.init
        |> insertCharacter idUser.Id
        |> ignore

        getCharactersForUser idUser.Id
    | None -> Seq.empty
    |> List.ofSeq

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