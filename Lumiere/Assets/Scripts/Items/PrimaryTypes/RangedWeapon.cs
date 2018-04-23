using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;

// TODO: Projectile Mechanics of some description.

class RangedWeapon : WeaponItem
{
    // Penetration of weapon.
    private int penetration;

    /// <summary>
    /// Base Weapon Constructor.
    /// <param name="penetration">How many enemies the weapon is allowed to go through with each shot.</param>
    /// </summary>
    public RangedWeapon(int penetration = 1) : base()
    {
        InitAttackData(penetration);
    }

    private void InitAttackData(int penetration)
    {
        this.penetration = penetration;
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
    /// <param name="range">The range of the swing outward from the entity. Negitive number behavior and 0 behavior not defined.</param>
    /// <param name="penetration">Arc of the swing of this melee weapon. Negitive number behavior and values over 360 are not defined.</param>
    public RangedWeapon(Sprite gui, Sprite ground, string newName, string newDesc, double val, ItemRarity rareness, int itemQuantity = 1, int newMaxStack = 1, int itemID = -1, string useAction = null, float dmg = 5.0f, double rateOF = 1.0, double range = 20.0, int penetration = 1) : base(gui, ground, newName, newDesc, val, rareness, itemQuantity, newMaxStack, itemID, useAction, dmg, rateOF, range)
    {
        InitAttackData(penetration);
    }

    override public GameItem clone()
    {
        return new RangedWeapon (
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
            this.rof,
            this.penetration);
    }

    /// <summary>
    /// Penetration Getter and Setter
    /// </summary>
    public int Penetration
    {
        get
        {
            return penetration;
        }

        set
        {
            penetration = value;
        }
    }

}
