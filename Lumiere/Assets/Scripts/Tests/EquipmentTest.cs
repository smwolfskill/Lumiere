using NUnit.Framework;
using NUnit.Framework.Internal;

class EquipmentTest
{
    private EquipmentManager equips;
    private ArmorItem chestPlate;
    private ArmorItem ring;
    private UsableItem weapon;
    private UsableItem potion;

    /// <summary>
    /// Setup phase for initilizing tests.
    /// </summary>
    [SetUp]
    public void Init()
    {
        equips = new EquipmentManager();
        chestPlate = new ArmorItem(EquipmentManager.EquipSlot.CHEST, 10, 10, 2);
        ring = new ArmorItem(EquipmentManager.EquipSlot.RING, 1, 20, 10);

        potion = new UsableItem
        {
            Name = "Potiony Boi"
        };

        weapon = new UsableItem
        {
            Name = "Cool Weapon Thing"
        };  // This is really neat btw.
    }

    /// <summary>
    /// Tests that the equipment menu can equip items.
    /// </summary>
    [Test]
    public void EquipmentEquip()
    {
        // Test chest plate is equipped to chest slot.
        Assert.IsTrue(equips.Equip(chestPlate));
        EquippableItem chestAgain = equips.GetEquippedItem(EquipmentManager.EquipSlot.CHEST);
        Assert.IsNotNull(chestAgain);
        Assert.IsInstanceOf<ArmorItem>(chestAgain);
        Assert.Equals(10, ((ArmorItem)chestAgain).Armor);

        // Test that the ring is equipped to the ring slot.
        Assert.IsTrue(equips.Equip(ring));
        EquippableItem ringAgain = equips.GetEquippedItem(EquipmentManager.EquipSlot.RING);
        Assert.IsNotNull(ringAgain);

        // Test that an item cannot be equipped to a slot with an item already.
        Assert.IsFalse(equips.Equip(chestPlate));
    }

    /// <summary>
    /// Tests that the equipment menu can add hotbar usables.
    /// </summary>
    [Test]
    public void EquipmentHotbarAdd()
    {
        // Test that the potion is slotted in slot 1.
        Assert.IsNull(equips.GetHotbarItem(1));
        Assert.IsTrue(equips.AddHotBarItem(potion, 1));
        UsableItem potionNew = equips.GetHotbarItem(1);
        Assert.IsNotNull(potionNew);

        // Test that two items cannot slot to the same location.
        Assert.IsFalse(equips.AddHotBarItem(weapon, 1));

        // Test that the weapon can be slotted in slot 5 though.
        Assert.IsTrue(equips.AddHotBarItem(weapon, 5));

        // Test that it cannot be slotted above the hotbar size.
        Assert.IsFalse(equips.AddHotBarItem(weapon, 11));
    }

    /// <summary>
    /// Tests that the equipment menu can remove equipped items.
    /// </summary>
    [Test]
    public void EquipmentDequip()
    {
        // Add the two equippables.
        equips.Equip(ring);
        equips.Equip(chestPlate);

        // Attempt to remove from an empty slot, should return null.
        Assert.IsNull(equips.DeEquip(EquipmentManager.EquipSlot.GLOVES));

        // Remove from the ring slot, should be the ring item.
        EquippableItem ringAgain = equips.DeEquip(EquipmentManager.EquipSlot.RING);
        Assert.IsNotNull(ringAgain);
        Assert.IsNull(equips.GetEquippedItem(EquipmentManager.EquipSlot.RING));
    }

    /// <summary>
    /// Tests that the equipment menu can remove hotbar usables.
    /// </summary>
    [Test]
    public void EquipmentHotbarRemove()
    {
        // Add the two usables.
        equips.AddHotBarItem(weapon, 5);
        equips.AddHotBarItem(potion, 1);

        // Test that attempting to remove from an empty slot returns a null.
        Assert.IsNull(equips.RemoveHotBarItem(3));
        Assert.IsNull(equips.RemoveHotBarItem(11));

        // Remove the weapon, test it is, indeed, removed and the right item too.
        UsableItem removed = equips.RemoveHotBarItem(5);
        Assert.IsNotNull(removed);
        Assert.IsNull(equips.GetHotbarItem(5));
        Assert.Equals(0, weapon.Name.CompareTo(removed.Name));
    }

    /// <summary>
    /// Tests that the utility functions work as intended.
    /// </summary>
    [Test]
    public void EquipmentUtility()
    {
        // Equip the ring and chestplate.
        equips.Equip(ring);
        equips.Equip(chestPlate);

        // Tests that the damage adding utility works.
        Assert.Equals(12,equips.GetDamageModifier());

        // Tests that the speed adding utility works.
        Assert.Equals(30, equips.GetSpeedModifier());


        // Tests that the armor rating utility works.
        Assert.Equals(11, equips.GetArmorRating());
    }
}
