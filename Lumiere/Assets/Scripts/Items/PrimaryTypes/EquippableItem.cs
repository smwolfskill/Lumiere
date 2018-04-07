using UnityEngine;

/// <summary>
/// Representation of an item that can be Equipped by the player (at some point)
/// </summary>
public class EquippableItem : GameItem
{
    // Slot the item is equipped into.
    private EquipmentManager.EquipSlot slot;

    /// <summary>
    /// Base Equippable Item constructor.
    /// </summary>
    public EquippableItem() : base()
    {
        this.slot = EquipmentManager.EquipSlot.CHEST;
    }

    /// <summary>
    /// Constructor for an EquippableItem.
    /// Overrides the item quantity and max stack count to 1 since equippables cannot stack.
    /// </summary>
    /// <param name="gui">Sprite representing the item in the inventory interface.</param>
    /// <param name="ground">Sprite representing the item on the ground.</param>
    /// <param name="newName">Name of the item.</param>
    /// <param name="newDesc">Description of the item.</param>
    /// <param name="val">Value of the item in whatever unit.</param>
    /// <param name="rareness">Rarity of the item for rareness systems.</param>
    /// <param name="newMaxStack">Maximum times the item can stack in an inventory slot.</param>
    /// <param name="itemQuantity">Amount of items in this stack.</param>
    /// <param name="itemID">The ID of the item.</param>
    /// <param name="newSlot">Slot the equippable can be fit into. Defaults to chest.</param>
    public EquippableItem(Sprite gui, Sprite ground, string newName, string newDesc, double val, ItemRarity rareness, int itemQuantity = 1, int newMaxStack = 1, int itemID = -1, EquipmentManager.EquipSlot newSlot = EquipmentManager.EquipSlot.CHEST) : base(gui, ground, newName, newDesc, val, rareness, 1, 1, itemID)
    {
        this.slot = newSlot;
    }

    /// <summary>
    /// Constructor that only requires the slot information.
    /// </summary>
    /// <param name="newSlot">Slot to place the item into for equipment.</param>
    public EquippableItem(EquipmentManager.EquipSlot newSlot = EquipmentManager.EquipSlot.CHEST) : base()
    {
        this.slot = newSlot;
    }

    /// <summary>
    /// Getters and Setters for the slot.
    /// </summary>
    public EquipmentManager.EquipSlot Slot
    {
        get
        {
            return slot;
        }

        set
        {
            slot = value;
        }
    }
}