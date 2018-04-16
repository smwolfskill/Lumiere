using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;

// TODO: Create an "ATTACK" action for generic weapons to use.

/// <summary>
/// Class that represents a weapon of some description, weapons by default have damage and a rate of fire, everything else is determined by the specific weapon.
/// </summary>
public class WeaponItem : UsableItem
{
    // Proximity within which a player can attack a monster with this item
    public float AttackRange
    {
        get;
        protected set;
    }

    // Damage the weapon deals.
    protected float damage;

    // Rate of fire of the weapon.
    protected double rof;

    /// <summary>
    /// Base Weapon Constructor.
    /// </summary>
    public WeaponItem(float dmg = 5.0f, double rateOF = 1.0) : base()
    {
        this.damage = dmg;
        this.rof = rateOF;
    }

    /// <summary>
    /// Main constructor for a usable item.
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
    /// <param name="useAction">Use action for weapon, should default to an ATTACK action.</param>
    /// <param name="dmg">Damage of the weapon in question.</param>
    /// <param name="rateOF">Rate of fire of weapon (how many times per second it can be used).</param>
    public WeaponItem(Sprite gui, Sprite ground, string newName, string newDesc, double val, ItemRarity rareness, int itemQuantity = 1, int newMaxStack = 1, int itemID = -1, string useAction = null, float dmg = 5.0f, double rateOF = 1.0) : base(gui, ground, newName, newDesc, val, rareness, itemQuantity, newMaxStack, itemID, useAction)
    {
        this.damage = dmg;
        this.rof = rateOF;
    }

    override public GameItem clone()
    {
        return new WeaponItem (
            this.guiSprite,
            this.groundSprite,
            this.name,
            this.description,
            this.value,
            this.rarity,
            this.quantity,
            this.maxStacks,
            this.itemID,
            this.useAction,
            this.damage,
            this.rof);
    }

    /// <summary>
    /// Getters and Setter for damage.
    /// </summary>
    public float Damage
    {
        get
        {
            return damage;
        }

        set
        {
            damage = value;
        }
    }

    /// <summary>
    /// Getter and Setter for rate of fire.
    /// </summary>
    public double ROF
    {
        get
        {
            return rof;
        }

        set
        {
            rof = value;
        }
    }
}
