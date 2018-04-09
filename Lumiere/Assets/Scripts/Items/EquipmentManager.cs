using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EquipmentManager
{
    /// <summary>
    /// Enumeration that keeps track of slots items can be put into.
    /// Each item can correspond to a specific slot.
    /// </summary>
    public enum EquipSlot
    {
        HEAD,
        LEGS,
        CHEST,
        GLOVES,
        RING,
        NECK
    }

    //Holds the UI Panels for the Hotbar and Equipment
    public EquipmentPanel equipPanel;
    public HotbarPanel hotbarPanel;

    // Holds onto the current equipment in each possible slot (EquipSlots, 1 per) and the usable items in the hotbar.
    private EquippableItem[] equipment;
    private UsableItem[] hotbar;


    public int GetEquipmentSize()
    {
        return this.equipment.Length;
    }

    public int GetHotbarItemsSize()
    {
        return this.hotbar.Length;
    }

    private int _selection;
    private int HotbarSelection
    {
        get
        {
            return _selection;
        }
        set
        {
            if (value >= 1 && value <= hotbar.Length)
            {
                _selection = value;
            }
        }
    }

    /// <summary>
    /// Constructor for equipment manager.
    /// </summary>
    /// <param name="hotBarSize">Size of the player's hotbar, this is how many weapons usable items can be quick-swapped without opening a manager menu.</param>
    public EquipmentManager(int hotBarSize = 10, int selection = 1)
    {
        this.equipment = new EquippableItem[Enum.GetNames(typeof(EquipSlot)).Length];
        this.hotbar = new UsableItem[hotBarSize];
        Select(selection);
    }

    #region Equipping And Unequipping
    /// <summary>
    /// Attempts to equip a given item to the equipment manager.
    /// </summary>
    /// <param name="item">The equippable item attempting to be added.</param>
    /// <returns>True if successful, False otherwise.</returns>
    public bool Equip(EquippableItem item)
    {
        int index = (int) item.Slot;
        if (index < 0 || index >= equipment.Length) 
        {
            return false;
        }

        if (equipment[(int)item.Slot] != null)
        {
            return false;
        }

        equipment [(int) item.Slot] = item;
        UpdateEquipPanel();
        return true;
    }

    /// <summary>
    /// Attempts to dequip a given item from the equipment manager.
    /// </summary>
    /// <param name="slot">The slot type for the item being removed.</param>
    /// <returns>The item if it was removed, null if there was no item or removal somehow failed.</returns>
    public EquippableItem DeEquip(EquipSlot slot)
    {
        int index = (int) slot;
        if (index < 0 || index >= equipment.Length) 
        {
            return null;
        }
        EquippableItem itemToRemove = equipment [index];
        equipment [index] = null;
        UpdateEquipPanel();
        return itemToRemove;
    }

    /// <summary>
    /// Selects a hotbar item
    /// </summary>
    /// <param name="selection">hotbar slot</param>
    public void Select(int selection)
    {
        this.HotbarSelection = selection;
    }

    /// <summary>
    /// Fetches the item data associated with a particular item slot.
    /// </summary>
    /// <param name="slot">The slot of the equipped item.</param>
    /// <returns>The item if the slot contained an item. Null otherwise.</returns>
    public EquippableItem GetEquippedItem(EquipSlot slot)
    {
        int index = (int) slot;
        if (index < 0 || index >= equipment.Length) 
        {
            return null;
        }

        return equipment[index];
    }

    /// <summary>
    /// Fetches the item data associated with a particular item slot.
    /// </summary>
    /// <param name="slot">The index of the equipped item.</param>
    /// <returns>The item if the slot contained an item. Null otherwise.</returns>
    public EquippableItem GetEquippedItemFromIndex(int index)
    {
        if (index < 0 || index >= equipment.Length) 
        {
            return null;
        }

        return equipment[index];
    }

    /// <summary>
    /// Gets the currently selected index with respect to hotbar
    /// </summary>
    /// <returns>the currently selected index</returns>
    public int GetSelectedIndex()
    {
        return HotbarSelection - 1;
    }

    /// <summary>
    /// Gets the currently selected hotbar item
    /// </summary>
    /// <returns>the currently selected hotbar item</returns>
    public UsableItem GetSelectedItem()
    {
        return GetHotbarItem(GetSelectedIndex());
    }

    /// <summary>
    /// Attempts to get the usable item at a given hotbar index.
    /// </summary>
    /// <param name="index">Index on the hotbar the item is located.</param>
    /// <returns>The usable item if it exists, null if the hotbar slot is empty or out of bounds.</returns>
    public UsableItem GetHotbarItem(int index)
    {
        if (index < 0 || index >= hotbar.Length) 
        {
            return null;
        }

        return hotbar[index];
    }

    /// <summary>
    /// Attempts to add an item onto the hotbar.
    /// </summary>
    /// <param name="item">The item to be added.</param>
    /// <param name="index">The hotbar index to place the item.</param>
    /// <returns>True if successful, False if the slot already contains an item or the function fails.</returns>
    public bool AddHotBarItem(UsableItem item, int index)
    {
        if (index < 0 || index >= hotbar.Length) 
        {
            return false;
        }

        if (hotbar [index] != null) 
        {
            return false;
        }

        hotbar [index] = item;
        UpdateHotbarPanel();
        return true;
    }

    /// <summary>
    /// Attempts to remove an item from the hotbar.
    /// </summary>
    /// <param name="index">The index of the item to be removed.</param>
    /// <returns>The item being removed if it exists at that index, Null otherwise.</returns>
    public UsableItem RemoveHotBarItem(int index)
    {
        if (index < 0 || index >= hotbar.Length) 
        {
            return null;
        }

        UsableItem itemToRemove = hotbar [index];
        hotbar [index] = null;
        UpdateHotbarPanel();
        return itemToRemove;
    }
    #endregion

    #region Panel Updaters
    /// <summary>
    /// Redraws the Equipment panel
    /// </summary>
    private void UpdateEquipPanel()
    {
        if (equipPanel != null)
        {
            equipPanel.DrawEquipment();
        }
    }

    /// <summary>
    /// Redraws the Hotbar panel
    /// </summary>
    private void UpdateHotbarPanel()
    {
        if (hotbarPanel != null)
        {
            hotbarPanel.DrawHotbar();
        }
    }
    #endregion

    #region Utility Functions (quality of life for calculations)
    /// <summary>
    /// A quality of life function designed to sum all of the speed modifiers on equipped weapons and return the total percentage boost.
    /// </summary>
    /// <returns>Total percentage boost of speed modifiers on equipment.</returns>
    public double GetSpeedModifier()
    {
        double modifier = 0.0;
        for (int i = 0; i < Enum.GetNames(typeof(EquipSlot)).Length; i++)
        {
            EquippableItem currEquip = equipment[i];
            // Dummy Checks, two layers deep.
            if (currEquip != null)
            {
                if (currEquip is ArmorItem)
                {
                    ArmorItem armor = (ArmorItem)currEquip;
                    modifier += armor.SpeedModifier;
                }
            }
        }
        return modifier;
    }

    /// <summary>
    /// A quality of life function designed to sum all of the damage modifiers on equipped weapons and return the total percentage boost. 
    /// </summary>
    /// <returns>Total percentage boost of damage modifiers on equipment.</returns>
    public double GetDamageModifier()
    {
        double modifier = 0.0;
        for (int i = 0; i < Enum.GetNames(typeof(EquipSlot)).Length; i++)
        {
            EquippableItem currEquip = equipment[i];
            // Dummy Checks, two layers deep.
            if (currEquip != null)
            {
                if (currEquip is ArmorItem)
                {
                    ArmorItem armor = (ArmorItem)currEquip;
                    modifier += armor.DamageModifier;
                }
            }
        }
        return modifier;
    }

    /// <summary>
    /// A quality of life function designed to sum all of the armor ratings of each piece of armor.
    /// </summary>
    /// <returns>Total armor rating of all armor pieces.</returns>
    public double GetArmorRating()
    {
        double armorRating = 0.0;
        for (int i = 0; i < Enum.GetNames(typeof(EquipSlot)).Length; i++)
        {
            EquippableItem currEquip = equipment[i];
            // Dummy Checks, two layers deep.
            if (currEquip != null)
            {
                if (currEquip is ArmorItem)
                {
                    ArmorItem armor = (ArmorItem)currEquip;
                    armorRating += armor.Armor;
                }
            }
        }
        return armorRating;
    }
    #endregion
}
