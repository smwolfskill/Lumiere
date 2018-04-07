using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;

class ArmorItem : EquippableItem
{
    // Modifiers that the armor provides to the player, speed, damage, and an armor rating.
    private double speedModifier;
    private double armor;
    private double damageModifier;

    /// <summary>
    /// Armor constructor.
    /// <param name="slot">The slot the item can be placed into.</param>
    /// <param name="armor">Armor Value.</param>
    /// <param name="speedModifier">How much this armor affects the player's velocity. (0 = 0% boost; 100.0 = 100% boost)</param>
    /// <param name="damageModifier">How much this armor affects the player's damage output. (0 = 0% boost; 100.0 = 100% boost)</param>
    /// </summary>
    public ArmorItem(EquipmentManager.EquipSlot slot = EquipmentManager.EquipSlot.CHEST, double armor = 1.0, double speedModifier = 0.0, double damageModifier = 0.0) : base(slot)
    {
        InitArmor(armor, speedModifier, damageModifier);
    }

    /// <summary>
    /// Base Equippable Item constructor.
    /// <param name="armor">Armor Value.</param>
    /// <param name="speedModifier">How much this armor affects the player's velocity. (0 = 0% boost; 100.0 = 100% boost)</param>
    /// <param name="damageModifier">How much this armor affects the player's damage output. (0 = 0% boost; 100.0 = 100% boost)</param>
    /// </summary>
    public ArmorItem(double armor = 1.0, double speedModifier = 0.0, double damageModifier = 0.0) : base()
    {
        InitArmor(armor, speedModifier, damageModifier);
    }

    /// <summary>
    /// Initilizes the armor data.
    /// </summary>
    /// <param name="armor">Armor Value.</param>
    /// <param name="speedModifier">How much this armor affects the player's velocity. (0 = 0% boost; 100.0 = 100% boost)</param>
    /// <param name="damageModifier">How much this armor affects the player's damage output. (0 = 0% boost; 100.0 = 100% boost)</param>
    private void InitArmor(double armor, double speedModifier, double damageModifier)
    {
        this.armor = armor;
        this.speedModifier = speedModifier;
        this.damageModifier = damageModifier;
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
    public ArmorItem(Sprite gui, Sprite ground, string newName, string newDesc, double val, ItemRarity rareness, int itemQuantity = 1, int newMaxStack = 1, int itemID = -1, EquipmentManager.EquipSlot newSlot = EquipmentManager.EquipSlot.CHEST, double armor = 1.0, double speedModifier = 0.0, double damageModifier = 0.0) : base(gui, ground, newName, newDesc, val, rareness, 1, 1, itemID, newSlot)
    {
        InitArmor(armor, speedModifier, damageModifier);
    }

    /// <summary>
    /// Getter and Setter for the speed modifier.
    /// </summary>
    public double SpeedModifier
    {
        get
        {
            return speedModifier;
        }

        set
        {
            speedModifier = value;
        }
    }

    /// <summary>
    /// Getter and Setter for the armor rating.
    /// </summary>
    public double Armor
    {
        get
        {
            return armor;
        }

        set
        {
            armor = value;
        }
    }

    /// <summary>
    /// Getter and Setter for the damage modifier.
    /// </summary>
    public double DamageModifier
    {
        get
        {
            return damageModifier;
        }

        set
        {
            damageModifier = value;
        }
    }
}
