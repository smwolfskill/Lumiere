using UnityEngine;

/// <summary>
/// Representation of an item that can be Equipped by the player (at some point)
/// </summary>
public class EquippableItem : GameItem
{
    // Stats will be added here at some point to store additional information about these sorts of items and how they affect players.

    /// <summary>
    /// Copy constructor
    /// </summary>
    /// <param name="other">other item</param>
    public EquippableItem(EquippableItem other) : base(other)
    {
        this.EquipSlot = other.EquipSlot;
    }

    /// <summary>
    /// Enumeration that keeps track of slots items can be put into.
    /// </summary>
    public enum EquipmentSlot
    {
        HEAD,
        WEAPON,
        LEGS,
        CHEST,
        GLOVES,
        RING,
        NECK
    }

    public EquipmentSlot EquipSlot
    {
        get; private set;
    }
}