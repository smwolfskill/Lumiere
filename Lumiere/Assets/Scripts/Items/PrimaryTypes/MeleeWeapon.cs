using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnityEngine;

// TODO: An action that describes a melee attack.

/// <summary>
/// A specific weapon that works in close range.
/// Close range weapons have a swinging arc that has a range (how far out it swings) and an angle (measured out in both directions from where the entity is looking).
/// </summary>
class MeleeWeapon : WeaponItem
{
    // Radius and arc of swing.
    private double radius;
    private double arc;

    /// <summary>
    /// Base Weapon Constructor.
    /// </summary>
    /// <param name="radius">The range of the swing outward from the entity. Negitive number behavior and 0 behavior not defined.</param>
    /// <param name="arc">Arc of the swing of this melee weapon. Negitive number behavior and values over 360 are not defined.</param>
    public MeleeWeapon(double radius = 5.0, double arc = 90.0) : base()
    {
        InitAttackData(radius, arc);
    }

    private void InitAttackData(double radius, double arc)
    {
        this.radius = radius;
        this.arc = arc;
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
    /// <param name="radius">The range of the swing outward from the entity. Negitive number behavior and 0 behavior not defined.</param>
    /// <param name="arc">Arc of the swing of this melee weapon. Negitive number behavior and values over 360 are not defined.</param>
    public MeleeWeapon(Sprite gui, Sprite ground, string newName, string newDesc, double val, ItemRarity rareness, int itemQuantity = 1, int newMaxStack = 1, int itemID = -1, string useAction = null, float dmg = 5.0f, double rateOF = 1.0, double radius = 5.0, double arc = 90.0) : base(gui, ground, newName, newDesc, val, rareness, itemQuantity, newMaxStack, itemID, useAction, dmg, rateOF)
    {
        InitAttackData(radius, arc);
    }

    override public GameItem clone()
    {
        return new MeleeWeapon (
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
            this.radius,
            this.arc);
    }

    /// <summary>
    /// Radius Getter and Setter.
    /// </summary>
    public double Radius
    {
        get
        {
            return radius;
        }

        set
        {
            radius = value;
        }
    }

    /// <summary>
    /// Arc Getter and Setter.
    /// </summary>
    public double Arc
    {
        get
        {
            return arc;
        }

        set
        {
            arc = value;
        }
    }
}
