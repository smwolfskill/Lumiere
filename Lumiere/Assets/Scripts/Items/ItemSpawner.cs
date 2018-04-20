using System.Collections;
using System.Collections.Generic;
using System;

/// <summary>
/// Specialized static class designed to handle item generation algorithms.
/// </summary>
static class ItemSpawner
{
    // Essentially a const, it lists the liklyhood an item will be a certain rarity or higher based on the scale.
    private static readonly int[] rarityWeights = {20, 10, 5, 3, 1};
    private static readonly int floorRarityBoostThreshold = 5;  // How many floors before rarity increases.
    private static readonly string[] materials = { "Bronze", "Iron", "Steel", "Mythril", "Adamantium", "Luminite" };

    private static int lastID = 100;

    #region Item Generators
    /// <summary>
    /// Generation function designed to build an either a weapon or armor.
    /// </summary>
    /// <param name="seed">Seed to use for generation, useful for testing. Set this to system time during playtime.</param>
    /// <param name="quality">Quality modifier for items. Essentially represents how deep in the dungeon the player is. Pass this from the map class ideally.</param>
    /// <param name="rarity">Minimum item rarity, defaults to common. Higher rarity items have better stats implicitly.</param>
    /// <returns>An item, with name and description pre-set based on the item spec.</returns>
    public static GameItem GenerateItem(int seed, int quality, GameItem.ItemRarity rarity = GameItem.ItemRarity.COMMON)
    {
        // Some constants to use for calculations:
        int armorWeight = 2;    // Weighted likelyhood to pick this over the other.
        int weaponWeight = 1;   // Weighted likelyhood to pick this over the other.

        // Note we are using the system's random, and not Unity's in order to use a seed.
        Random random = new Random(seed);

        // Choose what type of thing to generate, armor, or a weapon?
        int val = random.Next(1, armorWeight + weaponWeight + 1);

        // Call other generation functions based on this.
        if (val >= weaponWeight)
            return GenerateWeapon(seed, quality, rarity);
        else
            return GenerateArmor(seed, quality, rarity);
    }

    /// <summary>
    /// Generation function designed to build an armor item for spawning. Can fit any slot.
    /// </summary>
    /// <param name="seed">Seed to use for generation, useful for testing. Set this to system time during playtime.</param>
    /// <param name="rarity">Minimum item rarity, defaults to common. Higher rarity items have better stats implicitly.</param>
    /// <returns>A weapon item, with name and description pre-set based on the item spec.</returns>
    public static ArmorItem GenerateArmor(int seed, int quality, GameItem.ItemRarity rarity = GameItem.ItemRarity.COMMON)
    {
        // Some constants to use for calculations:
        double[] minArmorRarityRatings = {1, 2, 3, 4, 5};           // Scaling factor for minimum based on rarity. Min * RarityRating = Absolute Minimum
        double[] minArmorRatings = {5, 10, 15, 5, 1, 1};            // Minimum rating per armor slot, in enum order.
        double[] maxArmorRatings = {20, 50, 100, 10, 5, 5};         // Maximum rating per armor slot, in enum order.
        
        double[] minSpeedRarityRatings = {1, 1.25, 1.5, 1.75, 2};   // Scaling factor for minimum based on rarity.
        double[] minSpeedRatings = {0, 1, 0, 0, 1, 1};              // Minimum rating per armor slot, in enum order.
        double[] maxSpeedRatings = {2, 5, 2, 1, 5, 5};              // Maximum rating per armor slot, in enum order.

        double[] minDamageRarityRatings = {1, 1.25, 1.5, 1.75, 2};  // Scaling factor for minimum based on rarity.
        double[] minDamageRatings = {0, 0, 0, 1, 1, 1};             // Minimum rating per armor slot, in enum order.
        double[] maxDamageRatings = {2, 2, 2, 5, 5, 5};             // Maximum rating per armor slot, in enum order. 

        // For reference, ENUM Order: HEAD, LEGS, CHEST, GLOVES, RING, NECK.

        // Note we are using the system's random, and not Unity's in order to use a seed.
        Random random = new Random(seed);

        // Select rarity first; this determines effectiveness.
        int totalWeight = 0;
        for (int i = 0; i < rarityWeights.Length; i++)
        {
            totalWeight += rarityWeights[i];
        }

        int randRarity = random.Next(totalWeight);
        int trueRarity = -1;
        totalWeight = rarityWeights[0];
        for (int i = 1; i < rarityWeights.Length && trueRarity == -1; i++)
        {
            if (randRarity < totalWeight)
                trueRarity = i - 1;

            totalWeight += rarityWeights[i];
        }

        // Rarity is guaurnteed to increase for every X floors of depth.
        trueRarity += (quality / floorRarityBoostThreshold);
        if (trueRarity > 4)
            trueRarity = 4;

        // Determine slot for armor.
        int trueSlot = random.Next(Enum.GetNames(typeof(EquipmentManager.EquipSlot)).Length);

        // Determine sprite info (somehow). TODO (I'm not sure how to do this)

        // Calculate the armor's values.
        double weightedMinArmor = minArmorRatings[trueSlot] * minArmorRarityRatings[trueRarity];
        if (weightedMinArmor > maxArmorRatings[trueSlot])
            weightedMinArmor = maxArmorRatings[trueSlot];

        double weightedMinSpeed = minSpeedRatings[trueSlot] * minSpeedRarityRatings[trueRarity];
        if (weightedMinArmor > maxSpeedRatings[trueSlot])
            weightedMinArmor = maxSpeedRatings[trueSlot];

        double weightedMinDamage = minDamageRatings[trueSlot] * minDamageRarityRatings[trueRarity];
        if (weightedMinArmor > maxDamageRatings[trueSlot])
            weightedMinArmor = maxDamageRatings[trueSlot];

        double armor = random.NextDouble() * (maxArmorRatings[trueSlot] - weightedMinArmor) + weightedMinArmor;
        double speed = random.NextDouble() * (maxSpeedRatings[trueSlot] - weightedMinSpeed) + weightedMinSpeed;
        double damage = random.NextDouble() * (maxDamageRatings[trueSlot] - weightedMinDamage) + weightedMinDamage;


        // Determine, based on rarity, if armor is allowed to have speed or damage.
        if (trueRarity < 1)
        {
            speed = 0;
            damage = 0;
        }
        else if (trueRarity < 2)
        {
            if (speed > damage)
                damage = 0;
            else
                speed = 0;
        }

        // Build the armor item.
        ArmorItem newItem = new ArmorItem((EquipmentManager.EquipSlot)trueSlot, armor, speed, damage)
        {
            Rarity = (GameItem.ItemRarity)trueRarity
        };

        // Generate a name.
        newItem.Name = GenerateArmorName(seed, newItem, minArmorRatings, maxArmorRatings, minSpeedRatings, maxSpeedRatings, minDamageRatings, maxDamageRatings);

        // Generate a description.
        newItem.Description = GenerateArmorDesc(seed, newItem);

        // Pick a value rating. (Should probably be another utility method).
        newItem.Value = GenerateItemValue(newItem);

        // Generate a unique ID (this might require some internal memory somewhere to keep track of what IDs have already been assigned, probably another utility function).
        newItem.ItemID = GenerateItemID();

        // Return the finished item.
        return newItem;
    }

    /// <summary>
    /// Generation function designed to build a weapon item of some sort. Can be a melee or ranged weapon.
    /// </summary>
    /// <param name="seed">Seed to use for generation, useful for testing. Set this to system time during playtime.</param>
    /// <param name="quality">Quality modifier for items. Essentially represents how deep in the dungeon the player is. Pass this from the map class ideally.</param>
    /// <param name="rarity">Minimum item rarity, defaults to common. Higher rarity items have better stats implicitly.</param>
    /// <returns>A weapon item, with name and description pre-set based on the item spec.</returns>
    public static WeaponItem GenerateWeapon(int seed, int quality, GameItem.ItemRarity rarity = GameItem.ItemRarity.COMMON)
    {
        // Some constants to use for calculations:
        int meleeWeight = 1;    // Weighted likelyhood to pick this over the other.
        int rangedWeight = 0;   // Weighted likelyhood to pick this over the other.

        // Note we are using the system's random, and not Unity's in order to use a seed.
        Random random = new Random(seed);

        // Choose what type of thing to generate, Ranged? Melee?
        int val = random.Next(1, meleeWeight + rangedWeight);

        // Generate basic stats, these apply to all weapons
        int rareVal = (int)rarity;
        double[] damageScale = { 1, 10, 25, 60, 135, 250 };             // Damage range dependent on rarity, For rarity i, minimum is damageScale[i]
        double[] rofMin = { 1, 2, 3, 4, 5 };    // TODO: PLEASE BALANCE THIS
        double[] rofMax = { 2, 4, 6, 8, 10 };
        double rangeMin = 1;
        double rangeMax = 5;    // Ranged weapon range is x10 this.

        // Get Damage Value; utilizes quality and rarity to help it scale.
        double damage = random.NextDouble() * (damageScale[rareVal + 1] - damageScale[rareVal]) + damageScale[rareVal];
        damage = damage + damage * (quality / floorRarityBoostThreshold);

        // Get Range Value; depdenent on nothing.
        double range = random.NextDouble() * (rangeMax - rangeMin) + rangeMin;

        // Get Rate Of Fire; higher rarities increase this value.
        double rof = random.NextDouble() * (rofMax[rareVal] - rofMin[rareVal]) + rofMin[rareVal];

        // Call other generation functions based on this.
        if (val >= rangedWeight)
            return GenerateMeleeWeapon(seed, quality, damage, range, rof, (int)rarity);
        else
            return GenerateRangedWeapon(seed, quality, damage, range * 10, rof, (int)rarity);
    }

    /// <summary>
    /// Generation function designed to build specifically a melee weapon.
    /// </summary>
    /// <param name="seed">Seed to use for generation, useful for testing. Set this to system time during playtime.</param>
    /// <param name="quality">Quality modifier for items. Essentially represents how deep in the dungeon the player is. Pass this from the map class ideally.</param>
    /// <param name="rarity">Minimum item rarity, defaults to common. Higher rarity items have better stats implicitly.</param>
    /// <returns>A melee weapon with pre-set name and description.</returns>
    public static MeleeWeapon GenerateMeleeWeapon(int seed, int quality, double damage, double range, double rof, int rareVal)
    {
        // Note we are using the system's random, and not Unity's in order to use a seed.
        Random random = new Random(seed);

        // Get Arc Value (Attacks may not even use this but do it anyways); higher rarities increase this value.
        double[] arcMin = {30, 60, 90, 120, 150};
        double[] arcMax = {60, 90, 120, 150, 180};

        double arc = random.NextDouble() * (arcMax[rareVal] - arcMin[rareVal]) + arcMin[rareVal];

        // Now assemble and do the other important steps.
        MeleeWeapon newItem = new MeleeWeapon(arc)
        {
            Rarity = (GameItem.ItemRarity)rareVal,
            Damage = (float)damage,
            ROF = rof,
            AttackRange = range
        };

        // Generate a name.
        newItem.Name = GenerateWeaponName(seed, newItem);

        // Generate a description.
        newItem.Description = GenerateWeaponDesc(seed, newItem);

        // Pick a value rating. (Should probably be another utility method).
        newItem.Value = GenerateItemValue(newItem);

        // Generate a unique ID (this might require some internal memory somewhere to keep track of what IDs have already been assigned, probably another utility function).
        newItem.ItemID = GenerateItemID();

        // Return the finished item.
        return newItem;
    }

    /// <summary>
    /// Generation function designed to build specifically a ranged weapon.
    /// </summary>
    /// <param name="seed">Seed to use for generation, useful for testing. Set this to system time during playtime.</param>
    /// <param name="quality">Quality modifier for items. Essentially represents how deep in the dungeon the player is. Pass this from the map class ideally.</param>
    /// <param name="rarity">Minimum item rarity, defaults to common. Higher rarity items have better stats implicitly.</param>
    /// <returns>A ranged weapon with pre-set name and description.</returns>
    public static RangedWeapon GenerateRangedWeapon(int seed, int quality, double damage, double range, double rof, int rareVal)
    {
        // Note we are using the system's random, and not Unity's in order to use a seed.
        Random random = new Random(seed);

        // Get Penetration Value (how many enemies an attack can pierce before self deleting); higher rarities increase this value.
        int[] penMin = { 1, 1, 2, 2, 3 };
        int[] penMax = { 2, 3, 4, 5, 6 };

        int penetration = random.Next(penMin[rareVal], penMax[rareVal]+1);

        // Now assemble and do the other important steps.
        RangedWeapon newItem = new RangedWeapon(penetration)
        {
            Rarity = (GameItem.ItemRarity)rareVal,
            Damage = (float)damage,
            ROF = rof,
            AttackRange = range
        };

        // Generate a name.
        newItem.Name = GenerateWeaponName(seed, newItem);

        // Generate a description.
        newItem.Description = GenerateWeaponDesc(seed, newItem);

        // Pick a value rating. (Should probably be another utility method).
        newItem.Value = GenerateItemValue(newItem);

        // Generate a unique ID (this might require some internal memory somewhere to keep track of what IDs have already been assigned, probably another utility function).
        newItem.ItemID = GenerateItemID();

        // Return the finished item.
        return newItem;
    }

    /// <summary>
    /// Generation function designed to build an array of game items, can be potions, equipment, or just misc stuff. 
    /// Function is primarily used for spawning more than one thing in a treasure room.
    /// </summary>
    /// <param name="seed">Seed to use for generation, useful for testing. Set this to system time during playtime.</param>
    /// <param name="quality">Quality modifier for items. Essentially represents how deep in the dungeon the player is. Pass this from the map class ideally.</param>
    /// <param name="min">Minimum amount of items that are allowed to spawn, inclusive.</param>
    /// <param name="max">Maximum amount of items that are allowed to spawn, inclusive.</param>
    /// <param name="rarity">Minimum item rarity, defaults to common. Higher rarity items have better stats implicitly.</param>
    /// <returns>An equippable item, with name and description pre-set based on the item spec.</returns>
    public static GameItem[] GenerateLootBag(int seed, int quality, int min = 1, int max = 5, GameItem.ItemRarity rarity = GameItem.ItemRarity.COMMON)
    {
        // Random generation seed is set before doing anyhting else, neat.
        Random random = new Random(seed);

        // Size up the situation.
        int size = random.Next(min, max+1);
        GameItem[] lootBag = new GameItem[size];

        // Generate and fill the bag.
        for (int i = 0; i < size; i++)
        {
            lootBag[i] = GenerateItem(seed, quality, rarity);
        }

        return lootBag;
    }
    #endregion

    #region Additional Utility Functions
    /// <summary>
    /// Takes a weapon and generates a name based on the item's stats.
    /// </summary>
    /// <param name="seed">Seed for generating the name. Should be set to system time during live gameplay.</param>
    /// <param name="item">The item data to find a name for. Name should fit the data.</param>
    /// <returns>A potential name for the item.</returns>
    public static string GenerateWeaponName(int seed, WeaponItem item)
    {
        // Use the other name generator functions based on what item we have.
        if (item is MeleeWeapon)
            return GenerateMeleeName(seed, item as MeleeWeapon);
        else
            return GenerateRangedName(seed, item as RangedWeapon);
    }

    /// <summary>
    /// Helper function for generating a melee weapon name.
    /// </summary>
    /// <param name="seed">Seed for the generator.</param>
    /// <param name="item">The item that needs a name.</param>
    /// <returns>A potential name for the given item.</returns>
    private static string GenerateMeleeName(int seed, MeleeWeapon item)
    {
        // Name Pool.
        string[] types = { "Dagger", "Shortsword", "Longsword", "Two-Handed Sword" };

        // Based on the range.
        double[] typeCutoffRange = { 1, 2, 3, 5 };

        // Prefixes, based on item rarity.
        string[] prefixesCommon = { "Basic", "Average", "Mundane", "Blunt", "Trivial", "Ugly" };
        string[] prefixesUncommon = { "Interesting", "Cool", "Sharp", "Pointy", "Stabby", "Zealous" };
        string[] prefixesRare = { "Exciting", "Strong", "True", "Knight's", "Rare", "Lordly" };
        string[] prefixesEpic = { "Powerful", "Dangerous", "Violent", "Epic", "Killer", "Unstoppable" };
        string[] prefixesLegend = { "Divine", "Legendary", "Godly", "Unbelivable", "Overpowered", "Mythical" };

        // Based on rarity, this is the likelyhood a suffix will NOT appear on the weapon.
        int[] suffixThreshold = { 90, 75, 40, 0, 0 };
        string[] suffixs = { "Fabulousness", "Coolness", "Swagger", "The Monk", "The Dragon", "The Burrito", "Dangerousness", "Pointyness", "Aggressiveness", "Revengence" };

        return CompileWeaponName(seed, item, types, typeCutoffRange, prefixesCommon, prefixesUncommon, prefixesRare, prefixesEpic, prefixesLegend, suffixThreshold, suffixs);
    }

    /// <summary>
    /// Helper function for generating a ranged weapon name.
    /// </summary>
    /// <param name="seed">Seed for the generator.</param>
    /// <param name="item">The item that needs a name.</param>
    /// <returns>A potential name for the given item.</returns>
    private static string GenerateRangedName(int seed, RangedWeapon item)
    {
        // Name Pool.
        string[] types = { "Shortbow, Crossbow, Composite Bow, Longbow" };

        // Based on the range.
        double[] typeCutoffRange = { 10, 20, 30, 40 };

        // Prefixes, based on item rarity.
        string[] prefixesCommon = { "Basic", "Average", "Mundane", "Poorly Strung", "Trivial", "Ugly" };
        string[] prefixesUncommon = { "Interesting", "Cool", "Well Strung", "Decent", "Accurate", "Zealous" };
        string[] prefixesRare = { "Exciting", "Strong", "True", "Ranger's", "Rare", "Lordly" };
        string[] prefixesEpic = { "Powerful", "Dangerous", "Violent", "Epic", "Killer", "Unstoppable" };
        string[] prefixesLegend = { "Divine", "Legendary", "Godly", "Unbelivable", "Overpowered", "Unreal" };

        // Based on rarity, this is the likelyhood a suffix will NOT appear on the weapon.
        int[] suffixThreshold = { 90, 75, 40, 0, 0 };
        string[] suffixs = { "Fabulousness", "Shootyness", "Swagger", "The Ninja", "The Dragon", "The Cactus", "Dangerousness", "Rangedness", "Aggressiveness", "Revengence" };

        return CompileWeaponName(seed, item, types, typeCutoffRange, prefixesCommon, prefixesUncommon, prefixesRare, prefixesEpic, prefixesLegend, suffixThreshold, suffixs);
    }

    /// <summary>
    /// Helper function for generating weapon names.
    /// </summary>
    /// <param name="seed">Seed for generation.</param>
    /// <param name="item">Item data for the name.</param>
    /// <param name="types">A list of item types, this is the primary name for the item.</param>
    /// <param name="typeCutoffRange">Range cutoffs, must be the same size as types or it won't work.</param>
    /// <param name="prefixesCommon">Prefixes for common weapons.</param>
    /// <param name="prefixesUncommon">Prefixes for uncommon weapons.</param>
    /// <param name="prefixesRare">Prefixes for rare weapons.</param>
    /// <param name="prefixesEpic">Prefixes for epic weapons.</param>
    /// <param name="prefixesLegend">Prefixes for legendary weapons.</param>
    /// <param name="suffixThreshold">Threshold based on rarity that a suffix will actually be placed onto an item. Must be size of rarity enum count.</param>
    /// <param name="suffixs">List of possible suffixes.</param>
    /// <returns>Name for the compiled item.</returns>
    private static string CompileWeaponName(int seed, WeaponItem item, string[] types, double[] typeCutoffRange, string[] prefixesCommon, string[] prefixesUncommon, string[] prefixesRare, string[] prefixesEpic, string[] prefixesLegend, int[] suffixThreshold, string[] suffixs)
    {
        // Note we are using the system's random, and not Unity's in order to use a seed.
        Random random = new Random(seed);

        // Generate the type;
        string type = "NOPE";
        double range = item.AttackRange;

        // Iterate through list of types, type is based on range.
        for (int i = 0; i < types.Length && type.CompareTo("NOPE") == 0; i++)
        {
            if (range <= typeCutoffRange[i])
            {
                type = types[i];
            }
        }

        return ApplyNameModifiers(seed, item, type, prefixesCommon, prefixesUncommon, prefixesRare, prefixesEpic, prefixesLegend, suffixThreshold, suffixs);
    }

    /// <summary>
    /// Takes a piece of armor and generates a name based on the item's stats.
    /// </summary>
    /// <param name="seed">Seed for generating the name. Should be set to system time during live gameplay.</param>
    /// <param name="item">The item data to find a name for. Name should fit the data.</param>
    /// <returns>A potential name for the item.</returns>
    public static string GenerateArmorName(int seed, ArmorItem item, double[] minArm, double[] maxArm, double[] minSpd, double[] maxSpd, double[] minDmg, double[] maxDmg)
    {
        // Name of object.
        string name = "";

        // Determined based on the slot the item occupies.
        switch (item.Slot)
        {
            case EquipmentManager.EquipSlot.CHEST:
                name = GenerateNameChestPlate(seed, item, minArm[2], maxArm[2]);
                break;

            case EquipmentManager.EquipSlot.GLOVES:
                name = GenerateNameGloves(seed, item, minArm[3], maxArm[3]);
                break;

            case EquipmentManager.EquipSlot.HEAD:
                name = GenerateNameHead(seed, item, minArm[0], maxArm[0]);
                break;

            case EquipmentManager.EquipSlot.LEGS:
                name = GenerateNameLegs(seed, item, minArm[1], maxArm[1]);
                break;

            case EquipmentManager.EquipSlot.NECK:
                name = GenerateNameNeck(seed, item, minArm[5], maxArm[5]);
                break;

            case EquipmentManager.EquipSlot.RING:
                name = GenerateNameRing(seed, item, minArm[4], maxArm[4]);
                break;
        }

        return name;
    }

    /// <summary>
    /// A helper function that builds a name for a chestplate item.
    /// </summary>
    /// <param name="seed">Random number generation seed.</param>
    /// <param name="item">Item a name is being generated for.</param>
    /// <param name="minArmor">Minimum possible armor value for this item.</param>
    /// <param name="maxArmor">Maximum possible armor value for this item.</param>
    /// <returns>A potential name for the given item.</returns>
    private static string GenerateNameChestPlate(int seed, ArmorItem item, double minArmor, double maxArmor)
    {
        // Types.
        string[] types = { "Chainmail", "Chestplate", "Platemail" };

        // Prefixes, based on item rarity.
        string[] prefixesCommon = { "Peasant's", "Dirty", "Weak", "Lame", "Unimpressive." };
        string[] prefixesUncommon = { "Soldier's", "Clean", "Decent", "Interesting" };
        string[] prefixesRare = { "Knight's", "Powerful", "Sparkling", "Cool Looking" };
        string[] prefixesEpic = { "Hero's", "Extremely Strong", "Proud" };
        string[] prefixesLegend = { "King's", "Titan's", "Gorgeous", "Godly" };

        return NameArmor(seed, item, minArmor, maxArmor, types, prefixesCommon, prefixesUncommon, prefixesRare, prefixesEpic, prefixesLegend);
    }

    /// <summary>
    /// A helper function that builds a name for a glove item.
    /// </summary>
    /// <param name="seed">Random number generation seed.</param>
    /// <param name="item">Item a name is being generated for.</param>
    /// <param name="minArmor">Minimum possible armor value for this item.</param>
    /// <param name="maxArmor">Maximum possible armor value for this item.</param>
    /// <returns>A potential name for the given item.</returns>
    private static string GenerateNameGloves(int seed, ArmorItem item, double minArmor, double maxArmor)
    {
        // Types.
        string[] types = { "Gloves", "Gauntlets", "Bracers" };

        // Prefixes, based on item rarity.
        string[] prefixesCommon = { "Peasant's", "Dirty", "Weak", "Lame", "Unimpressive." };
        string[] prefixesUncommon = { "Soldier's", "Clean", "Decent", "Interesting" };
        string[] prefixesRare = { "Knight's", "Powerful", "Sparkling", "Cool Looking" };
        string[] prefixesEpic = { "Hero's", "Extremely Strong", "Proud" };
        string[] prefixesLegend = { "King's", "Titan's", "Gorgeous", "Godly" };

        return NameArmor(seed, item, minArmor, maxArmor, types, prefixesCommon, prefixesUncommon, prefixesRare, prefixesEpic, prefixesLegend);
    }

    /// <summary>
    /// A helper function that builds a name for a leg item.
    /// </summary>
    /// <param name="seed">Random number generation seed.</param>
    /// <param name="item">Item a name is being generated for.</param>
    /// <param name="minArmor">Minimum possible armor value for this item.</param>
    /// <param name="maxArmor">Maximum possible armor value for this item.</param>
    /// <returns>A potential name for the given item.</returns>
    private static string GenerateNameLegs(int seed, ArmorItem item, double minArmor, double maxArmor)
    {
        // Types.
        string[] types = { "Chainleggings", "Plateleggings", "Chaps" };

        // Prefixes, based on item rarity.
        string[] prefixesCommon = { "Peasant's", "Dirty", "Weak", "Lame", "Unimpressive." };
        string[] prefixesUncommon = { "Soldier's", "Clean", "Decent", "Interesting" };
        string[] prefixesRare = { "Knight's", "Powerful", "Sparkling", "Cool Looking" };
        string[] prefixesEpic = { "Hero's", "Extremely Strong", "Proud" };
        string[] prefixesLegend = { "King's", "Titan's", "Gorgeous", "Godly" };

        return NameArmor(seed, item, minArmor, maxArmor, types, prefixesCommon, prefixesUncommon, prefixesRare, prefixesEpic, prefixesLegend);
    }

    /// <summary>
    /// A helper function that builds a name for a ring item.
    /// </summary>
    /// <param name="seed">Random number generation seed.</param>
    /// <param name="item">Item a name is being generated for.</param>
    /// <param name="minArmor">Minimum possible armor value for this item.</param>
    /// <param name="maxArmor">Maximum possible armor value for this item.</param>
    /// <returns>A potential name for the given item.</returns>
    private static string GenerateNameRing(int seed, ArmorItem item, double minArmor, double maxArmor)
    {
        // Types.
        string[] types = { "Ring", "Diamond Ring", "Sapphire Ring", "Ruby Ring", "Emerald Ring" };

        // Prefixes, based on item rarity.
        string[] prefixesCommon = { "Peasant's", "Dirty", "Weak", "Lame", "Unimpressive." };
        string[] prefixesUncommon = { "Soldier's", "Clean", "Decent", "Interesting" };
        string[] prefixesRare = { "Knight's", "Powerful", "Sparkling", "Cool Looking" };
        string[] prefixesEpic = { "Hero's", "Extremely Strong", "Proud" };
        string[] prefixesLegend = { "King's", "Titan's", "Gorgeous", "Godly" };

        return NameArmor(seed, item, minArmor, maxArmor, types, prefixesCommon, prefixesUncommon, prefixesRare, prefixesEpic, prefixesLegend);
    }

    /// <summary>
    /// A helper function that builds a name for an amulet item.
    /// </summary>
    /// <param name="seed">Random number generation seed.</param>
    /// <param name="item">Item a name is being generated for.</param>
    /// <param name="minArmor">Minimum possible armor value for this item.</param>
    /// <param name="maxArmor">Maximum possible armor value for this item.</param>
    /// <returns>A potential name for the given item.</returns>
    private static string GenerateNameNeck(int seed, ArmorItem item, double minArmor, double maxArmor)
    {
        // Types.
        string[] types = { "Necklace", "Amulet", "Diamond Encrusted Amulet", "Sapphire Encrusted Amulet", "Ruby Encrusted Amulet", "Emerald Encrusted Amulet" };

        // Prefixes, based on item rarity.
        string[] prefixesCommon = { "Peasant's", "Dirty", "Weak", "Lame", "Unimpressive." };
        string[] prefixesUncommon = { "Soldier's", "Clean", "Decent", "Interesting" };
        string[] prefixesRare = { "Knight's", "Powerful", "Sparkling", "Cool Looking" };
        string[] prefixesEpic = { "Hero's", "Extremely Strong", "Proud" };
        string[] prefixesLegend = { "King's", "Titan's", "Gorgeous", "Godly" };

        return NameArmor(seed, item, minArmor, maxArmor, types, prefixesCommon, prefixesUncommon, prefixesRare, prefixesEpic, prefixesLegend);
    }

    /// <summary>
    /// A helper function that builds a name for a chestplate item.
    /// </summary>
    /// <param name="seed">Random number generation seed.</param>
    /// <param name="item">Item a name is being generated for.</param>
    /// <param name="minArmor">Minimum possible armor value for this item.</param>
    /// <param name="maxArmor">Maximum possible armor value for this item.</param>
    /// <returns>A potential name for the given item.</returns>
    private static string GenerateNameHead(int seed, ArmorItem item, double minArmor, double maxArmor)
    {
        // Types.
        string[] types = { "Helmet", "Coif", "Platehelm", "Mask" };

        // Prefixes, based on item rarity.
        string[] prefixesCommon = { "Peasant's", "Dirty", "Weak", "Lame", "Unimpressive." };
        string[] prefixesUncommon = { "Soldier's", "Clean", "Decent", "Interesting" };
        string[] prefixesRare = { "Knight's", "Powerful", "Sparkling", "Cool Looking" };
        string[] prefixesEpic = { "Hero's", "Extremely Strong", "Proud" };
        string[] prefixesLegend = { "King's", "Titan's", "Gorgeous", "Godly" };

        return NameArmor(seed, item, minArmor, maxArmor, types, prefixesCommon, prefixesUncommon, prefixesRare, prefixesEpic, prefixesLegend);
    }

    /// <summary>
    /// Applies the name data to the armor in question.
    /// </summary>
    /// <param name="seed">Random number generation seed.</param>
    /// <param name="item">Item a name is being generated for.</param>
    /// <param name="minArmor">Minimum possible armor value for this item.</param>
    /// <param name="maxArmor">Maximum possible armor value for this item.</param>
    /// <param name="types">Potential names an item can have.</param>
    /// <param name="prefixesCommon">Prefixes for common armors.</param>
    /// <param name="prefixesUncommon">Prefixes for uncommon armors.</param>
    /// <param name="prefixesRare">Prefixes for rare armors.</param>
    /// <param name="prefixesEpic">Prefixes for epic armors.</param>
    /// <param name="prefixesLegend">Prefixes for legendary armors.</param>
    /// <returns>A potential name for the given item.</returns>
    private static string NameArmor(int seed, ArmorItem item, double minArmor, double maxArmor, string[] types, string[] prefixesCommon, string[] prefixesUncommon, string[] prefixesRare, string[] prefixesEpic, string[] prefixesLegend)
    {
        // Note we are using the system's random, and not Unity's in order to use a seed.
        Random random = new Random(seed);

        // Material appliances.
        string name = types[random.Next(0, types.Length)];

        string material = GetMaterial(item, minArmor, maxArmor);

        name = material + " " + name;

        string[] suffixs = GetArmorSuffixs(item);

        // Suffixes will only generate if a piece of armor has a special property.
        int[] suffixThreshold;

        if (suffixs == null)
            suffixThreshold = new int[] { 101, 101, 101, 101, 101 };
        else
            suffixThreshold = new int[] { 0, 0, 0, 0, 0 };

        return ApplyNameModifiers(seed, item, name, prefixesCommon, prefixesUncommon, prefixesRare, prefixesEpic, prefixesLegend, suffixThreshold, suffixs);
    }

    /// <summary>
    /// Gets the material an equippable should be made out of given its current armor and max and min possible armors.
    /// </summary>
    /// <param name="item">The item in question.</param>
    /// <param name="minArmor">Lowest possible armor value.</param>
    /// <param name="maxArmor">Highest possible armor value.</param>
    /// <returns>The material name that an item should have.</returns>
    private static string GetMaterial(ArmorItem item, double minArmor, double maxArmor)
    {
        // Material goes after prefix but before type.
        double range = (maxArmor - minArmor);
        double portion = range / materials.Length;

        for (int i = materials.Length; i > 0; i--)
        {
            if (item.Armor >= minArmor + portion * i)
            {
                return materials[i];
            }
        }

         return materials[0];
    }

    /// <summary>
    /// Gets the suffix set for an armor item based on its stats.
    /// </summary>
    /// <param name="item">The piece of armor getting stats.</param>
    /// <returns>A list of possible suffixes, or null if no suffix should be applied.</returns>
    private static string[] GetArmorSuffixs(ArmorItem item)
    {
        // Get Dominant Trait.
        if (item.SpeedModifier > 1 && item.DamageModifier > 1)
        {
            return new string[] {"Equalibrium", "Prowess", "True Power", "The King" };
        }
        else if (item.SpeedModifier > 1)
        {
            return new string[] { "Dexterity", "Speed", "Evasion", "The Ranger", "The Assasin" };
        }
        else if (item.DamageModifier > 1)
        {
            return new string[] { "Strength", "Power", "Fortitude", "The Knight", "The Warrior" };
        }
        else

        // No Dominant Traits.
        return null;
    }

    /// <summary>
    /// Helper function for generating weapon names.
    /// </summary>
    /// <param name="seed">Seed for generation.</param>
    /// <param name="item">Item data for the name.</param>
    /// <param name="type">Item's base name, described by another function.</param>
    /// <param name="prefixesCommon">Prefixes for common weapons.</param>
    /// <param name="prefixesUncommon">Prefixes for uncommon weapons.</param>
    /// <param name="prefixesRare">Prefixes for rare weapons.</param>
    /// <param name="prefixesEpic">Prefixes for epic weapons.</param>
    /// <param name="prefixesLegend">Prefixes for legendary weapons.</param>
    /// <param name="suffixThreshold">Threshold based on rarity that a suffix will actually be placed onto an item. Must be size of rarity enum count.</param>
    /// <param name="suffixs">List of possible suffixes.</param>
    /// <returns>Name for the compiled item.</returns>
    private static string ApplyNameModifiers(int seed, GameItem item, string type, string[] prefixesCommon, string[] prefixesUncommon, string[] prefixesRare, string[] prefixesEpic, string[] prefixesLegend, int[] suffixThreshold, string[] suffixs)
    {
        // Note we are using the system's random, and not Unity's in order to use a seed.
        Random random = new Random(seed);

        // Pick a prefix and determine if a suffix is neccessary.
        string prefix;
        bool suffixReq = false;

        switch (item.Rarity)
        {
            case GameItem.ItemRarity.UNCOMMON:
                prefix = prefixesUncommon[random.Next(0, prefixesUncommon.Length)];
                suffixReq = (random.Next(0, 101) >= suffixThreshold[1]);
                break;
            case GameItem.ItemRarity.RARE:
                prefix = prefixesRare[random.Next(0, prefixesRare.Length)];
                suffixReq = (random.Next(0, 101) >= suffixThreshold[2]);
                break;

            case GameItem.ItemRarity.EPIC:
                prefix = prefixesEpic[random.Next(0, prefixesEpic.Length)];
                suffixReq = (random.Next(0, 101) >= suffixThreshold[3]);
                break;

            case GameItem.ItemRarity.LEGENDARY:
                prefix = prefixesLegend[random.Next(0, prefixesLegend.Length)];
                suffixReq = (random.Next(0, 101) >= suffixThreshold[4]);
                break;

            default:
                prefix = prefixesCommon[random.Next(0, prefixesCommon.Length)];
                suffixReq = (random.Next(0, 101) >= suffixThreshold[0]);
                break;
        }

        // Pick a suffix (if there should be one).
        string suffix = "";

        if (suffixReq)
        {
            suffix = "of " + suffixs[random.Next(suffixs.Length)];

            // Combine and return.
            return prefix + " " + type + " " + suffix;
        }

        return prefix + " " + type;
    }

    /// <summary>
    /// Takes a weapon and generates a description based on the item's stats.
    /// Requires a name to function correctly.
    /// </summary>
    /// <param name="seed">Seed for generating the description. Should be set to system time during live gameplay.</param>
    /// <param name="item">The item data to find a description for. Description should fit the data.</param>
    /// <returns>A potential description for the item.</returns>
    public static string GenerateWeaponDesc(int seed, WeaponItem item)
    {
        // This function is decisively more lazy than the others, as generating really unique definitions isn't really neccessary and we're short on time.
        if (item is MeleeWeapon)
            return "A sword, you can stab monsters with it.";
        else
            return "A ranged weapon, you can hit things at a distance with it.";
    }

    /// <summary>
    /// Takes a piece of armor and generates a description based on the item's stats.
    /// Requires a name to function correctly.
    /// </summary>
    /// <param name="seed">Seed for generating the description. Should be set to system time during live gameplay.</param>
    /// <param name="item">The item data to find a description for. Description should fit the data.</param>
    /// <returns>A potential description for the item.</returns>
    public static string GenerateArmorDesc(int seed, ArmorItem item)
    {
        // This function is decisively more lazy than the others, as generating really unique definitions isn't really neccessary and we're short on time.
        Random random = new Random(seed);
        switch (item.Slot)
        {
            case EquipmentManager.EquipSlot.CHEST:
                return "A piece of armor for your upper body and chest region. Provides defence.";

            case EquipmentManager.EquipSlot.LEGS:
                return "A piece of armor for your legs, its like pants but with more plating. Provides defence.";

            case EquipmentManager.EquipSlot.NECK:
                return "Fancy jewlery for your fancy neck; also probably protects from vampires. Provides defence.";

            case EquipmentManager.EquipSlot.GLOVES:
                return "Gauntlets, Gloves, etc, things you wear on your hands. Provides defence.";

            case EquipmentManager.EquipSlot.HEAD:
                return "A piece of armor for your head, probably a helmet, good for biking. Provides defence.";

            case EquipmentManager.EquipSlot.RING:
                return "A ring you can wear around your finger. Provides defence.";

        }
        return "A generic piece of armor, it can be used to defend you from harm.";
    }

    /// <summary>
    /// A function designed to determine wether or not a monster should drop an item.
    /// </summary>
    /// <param name="seed">Seed for generating the bool. Should be set to system time during live gameplay.</param>
    /// <param name="rewardFactor">A single integer representing the likelyhood this particular enemy should drop a reward. Ideally based both on floor level and monster power. Higher number = Larger Chance</param>
    /// <returns>True if the monster should drop a potion, False otherwise.</returns>
    public static bool DropPotion(int seed, int rewardFactor)
    {
        Random random = new Random(seed);
        if (random.Next(100) <= 1 + rewardFactor)
            return true;

        return false;
    }

    /// <summary>
    /// Generates the value for a given item.
    /// </summary>
    /// <param name="item">Item data in question, will usually be passed weapons or equipment; should have private helper functions for those.</param>
    /// <returns>The value the item should logically have. Note it is not random.</returns>
    public static double GenerateItemValue(GameItem item)
    {
        // Item value is based upon the item's rarity. TODO: Perhaps add more logic
        switch (item.Rarity)
        {
            case GameItem.ItemRarity.COMMON:
                return 50.0;

            case GameItem.ItemRarity.UNCOMMON:
                return 150.0;

            case GameItem.ItemRarity.RARE:
                return 350.0;

            case GameItem.ItemRarity.EPIC:
                return 650.0;

            case GameItem.ItemRarity.LEGENDARY:
                return 1500.0;
        }

        return 0.0;
    }

    /// <summary>
    /// Generates an ID for a given item. This function is likely to require some sort of closure or variable that keeps track of what the max selected ID is, given the static nature of this class.
    /// I imagine the MIN ID to start at 100, so everything below that can be used for special items as needed for testing.
    /// </summary>
    /// <returns>An unused ID for the item.</returns>
    public static int GenerateItemID()
    {
        lastID++;
        return lastID;
    }
    #endregion
}
