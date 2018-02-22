using UnityEngine;
using UnityEditor;

/// <summary>
/// Representation of an item that can be Equipped by the player (at some point)
/// </summary>
public class EquippableItem : GameItem
{
    // Stats will be added here at some point to store additional information about these sorts of items and how they affect players.

    /// <summary>
    /// Enumeration that keeps track of slots items can be put into.
    /// </summary>
    public enum EquipSlot
    {
        HEAD,
        WEAPON,
        LEGS,
        CHEST,
        GLOVES,
        RING,
        NECK
    }
}