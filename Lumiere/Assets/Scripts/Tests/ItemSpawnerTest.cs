using NUnit.Framework;
using NUnit.Framework.Internal;
using System;
using UnityEngine;

/// <summary>
/// Tests the ItemSpawner class.
/// </summary>
public class ItemSpawnerTest
{

    /// <summary>
    /// Setup phase for initilizing tests.
    /// </summary>
    [SetUp]
    public void Init()
    {
        if(ItemSpawner.itemSpriteLists == null)
        {
            ItemSpawner.LoadItemSprites();
        }
    }

    /// <summary>
    /// Check to make sure ID generation is in sequence
    /// </summary>
    [Test]
    public void ItemIDGeneration()
    {
        int firstID = ItemSpawner.GenerateItemID();
        int secondID = ItemSpawner.GenerateItemID();
        Assert.AreEqual(++firstID,secondID);
    }

    /// <summary>
    /// Checks the Generation of correct Item Values
    /// </summary>
    [Test]
    [TestCase(GameItem.ItemRarity.COMMON, 50.0)]
    [TestCase(GameItem.ItemRarity.UNCOMMON, 150.0)]
    [TestCase(GameItem.ItemRarity.RARE, 350.0)]
    [TestCase(GameItem.ItemRarity.EPIC, 650.0)]
    [TestCase(GameItem.ItemRarity.LEGENDARY, 1500.0)]
    public void ItemValue(GameItem.ItemRarity rarity, double value)
    {
    	GameItem valueTestItem = new GameItem(null, null, "Generic Item", "I am an Item.", 10, rarity, 5, 10, 2);
    	Assert.AreEqual(value,ItemSpawner.GenerateItemValue(valueTestItem));
    }

    /// <summary>
    /// Test bool of DropPotion by choosing values that force it to return true or false
    /// </summary>
    /*[Test]
    [TestCase(1, 100, true)]
    [TestCase(1000, -2, false)]
    [TestCase(1000, 100, true)]
    [TestCase(1, -2, false)]
    public void TestDropPoption(int seed, int reqardFactor, bool returnValue)
    {
    	Assert.AreEqual(returnValue, ItemSpawner.DropPotion(seed, reqardFactor));
    }*/

    /// <summary>
    /// Test the correct armor description is given based on the slot the armor holds
    /// </summary>
    [Test]
    [TestCase(EquipmentManager.EquipSlot.CHEST, "A piece of armor for your upper body and chest region. Provides defence.")]
    [TestCase(EquipmentManager.EquipSlot.LEGS, "A piece of armor for your legs, its like pants but with more plating. Provides defence.")]
    [TestCase(EquipmentManager.EquipSlot.NECK, "Fancy jewlery for your fancy neck; also probably protects from vampires. Provides defence.")]
    [TestCase(EquipmentManager.EquipSlot.GLOVES, "Gauntlets, Gloves, etc, things you wear on your hands. Provides defence.")]
    [TestCase(EquipmentManager.EquipSlot.HEAD, "A piece of armor for your head, probably a helmet, good for biking. Provides defence.")]
    [TestCase(EquipmentManager.EquipSlot.RING, "A ring you can wear around your finger. Provides defence.")]
    public void TestArmorDescription(EquipmentManager.EquipSlot slot, string description)
    {
    	ArmorItem armor = new ArmorItem(slot);
    	Assert.AreEqual(description, ItemSpawner.GenerateArmorDesc(5,armor));
    }

    /// <summary>
    /// Test the correct weapon description is given based on the type of weapon
    /// </summary>
    [Test]
    public void TestWeaponDescription()
    {
    	Assert.AreEqual("A sword, you can stab monsters with it.", ItemSpawner.GenerateWeaponDesc(5, new MeleeWeapon()));
    	Assert.AreEqual("A ranged weapon, you can hit things at a distance with it.", ItemSpawner.GenerateWeaponDesc(5, new RangedWeapon()));
    }

    /// <summary>
    /// Test the generation of armor name generation
    /// </summary>
    [Test]
    [TestCase(EquipmentManager.EquipSlot.CHEST, 2)]
    [TestCase(EquipmentManager.EquipSlot.LEGS, 1)]
    [TestCase(EquipmentManager.EquipSlot.NECK, 5)]
    [TestCase(EquipmentManager.EquipSlot.GLOVES, 3)]
    [TestCase(EquipmentManager.EquipSlot.HEAD, 0)]
    [TestCase(EquipmentManager.EquipSlot.RING, 4)]
    public void TestArmorName(EquipmentManager.EquipSlot slot, int armorRatingPosition)
    {
    	ArmorItem armor = new ArmorItem(slot);
    	double[] minArmor = new double[6];
    	double[] maxArmor = new double[6];
    	minArmor[armorRatingPosition] = 1;
    	maxArmor[armorRatingPosition] = 5;

    	double[] minSpeed = new double[6];
    	double[] maxSpeed = new double[6];
    	minSpeed[armorRatingPosition] = 1;
    	maxSpeed[armorRatingPosition] = 5;

    	double[] minDamage = new double[6];
    	double[] maxDamage = new double[6];
    	minDamage[armorRatingPosition] = 1;
    	maxDamage[armorRatingPosition] = 5;

    	string name = ItemSpawner.GenerateArmorName(5, armor, minArmor, maxArmor, minSpeed, maxSpeed, minDamage, maxDamage);
   	}

   	/// <summary>
    /// Test the generation of armor name generation by rarity
    /// </summary>
    [Test]
    [TestCase(EquipmentManager.EquipSlot.CHEST, 2, GameItem.ItemRarity.COMMON, "Peasant's")]
    [TestCase(EquipmentManager.EquipSlot.LEGS, 1, GameItem.ItemRarity.UNCOMMON, "Soldier's")]
    [TestCase(EquipmentManager.EquipSlot.NECK, 5, GameItem.ItemRarity.RARE, "Knight's")]
    [TestCase(EquipmentManager.EquipSlot.GLOVES, 3, GameItem.ItemRarity.EPIC, "Hero's")]
    [TestCase(EquipmentManager.EquipSlot.HEAD, 0, GameItem.ItemRarity.LEGENDARY, "King's")]
    public void TestArmorNameByRarity(EquipmentManager.EquipSlot slot, int armorRatingPosition, GameItem.ItemRarity rarity, string description)
    {
    	ArmorItem armor = new ArmorItem(rarity, slot);
    	double[] minArmor = new double[6];
    	double[] maxArmor = new double[6];
    	minArmor[armorRatingPosition] = 1;
    	maxArmor[armorRatingPosition] = 5;

    	double[] minSpeed = new double[6];
    	double[] maxSpeed = new double[6];
    	minSpeed[armorRatingPosition] = 1;
    	maxSpeed[armorRatingPosition] = 5;

    	double[] minDamage = new double[6];
    	double[] maxDamage = new double[6];
    	minDamage[armorRatingPosition] = 1;
    	maxDamage[armorRatingPosition] = 5;

    	string name = ItemSpawner.GenerateArmorName(5, armor, minArmor, maxArmor, minSpeed, maxSpeed, minDamage, maxDamage);
    	string[] ssize = name.Split(null);
    	Assert.AreEqual(ssize[0], description);
   	
   	}

   	/// <summary>
    /// Test the main loot bag function
    /// </summary>
    [Test]
    public void TestGenerateLootBag()
    {
    	GameItem[] loot = ItemSpawner.GenerateLootBag(5,5);
    	for(int i = 0; i < loot.Length; i++)
    	{
    		Assert.IsNotNull(loot[i]);
    	}
   	
   	}
}