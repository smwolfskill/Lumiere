using UnityEngine;
using UnityEditor;
using System.Collections.Generic;

/// <summary>
/// Representation of a generic item in the game.
/// </summary>
public class GameItem
{
    public static GameItem UNSET_ITEM = new GameItem ();
    protected Sprite guiSprite;       // Item in GUI.
    protected Sprite groundSprite;    // Item on ground.

    protected double value;           // Arbitrary value of item, to be used for trading
    protected string name;            // Item name.
    protected string description;     // Item description.
    protected ItemRarity rarity;      // Item rarity.
    protected int maxStacks;          // Maximum stack quantity.
    protected int quantity;           // Amount of items.
    protected int itemID;            // The ID of the item, each item needs a unique ID.
    /// <summary>
    /// Represents rarity of the item, probably for item color purposes.
    /// </summary>
    public enum ItemRarity
    {
        COMMON,
        UNCOMMON,
        RARE,
        EPIC,
        LEGENDARY
    }

    /// <summary>
    /// Default constructor, remove if unneccessary.
    /// </summary>
    public GameItem()
    {
        this.FillData();
    }

    /// <summary>
    /// Constructor with item information ready to go.
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
    public GameItem(Sprite gui, Sprite ground, string newName, string newDesc, double val, ItemRarity rareness, int itemQuantity = 1, int newMaxStack = 1, int itemID = -1)
    {
        this.FillData(gui, ground, newName, newDesc, val, rareness, itemQuantity, newMaxStack, itemID);
    }

    /// <summary>
    /// Fills out the class data for constructors.
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
    private void FillData(Sprite gui = null, Sprite ground = null, string newName = "Unknown", string newDesc = "ERROR: An unknown item.", double val = 0.0, ItemRarity rareness = ItemRarity.COMMON, int itemQuantity = 1, int newMaxStack = 1, int itemID = -1)
    {
        this.guiSprite = gui;
        this.groundSprite = ground;
        this.name = newName;
        this.description = newDesc;
        this.rarity = rareness;
        this.value = val;
        this.itemID = itemID;

        // Idiot proofing.
        if (newMaxStack < 1)
        {
            this.maxStacks = 1;
        }
        else
        {
            this.maxStacks = newMaxStack;
        }

        if (itemQuantity > newMaxStack)
        {
            this.quantity = newMaxStack;
        }
        else if (itemQuantity < 1)
        {
            this.quantity = 1;
        }
        else
        {
            this.quantity = itemQuantity;
        }
    }

    /// <summary>
    /// An equals method for comparing this item to other items. Auto generated.
    /// </summary>
    /// <param name="obj">Object being compared</param>
    /// <returns>True if the item is equal and False otherwise.</returns>
    public override bool Equals(object obj)
    {
        var item = obj as GameItem;
        return item != null &&
               EqualityComparer<Sprite>.Default.Equals(guiSprite, item.guiSprite) &&
               EqualityComparer<Sprite>.Default.Equals(groundSprite, item.groundSprite) &&
               value == item.value &&
               name == item.name &&
               description == item.description &&
               rarity == item.rarity &&
               maxStacks == item.maxStacks &&
               itemID == item.itemID;
    }

    public bool SetYet()
    {
        if(Object.ReferenceEquals(this, UNSET_ITEM))
        {
            return false;
        }
        return true;
    }

    #region Getters And Setters
    /// <summary>
    /// Getter and Setter for GUI Sprite.
    /// </summary>
    public Sprite GuiSprite
    {
        get
        {
            return guiSprite;
        }

        set
        {
            guiSprite = value;
        }
    }

    /// <summary>
    /// Getter and Setter for Ground Sprite.
    /// </summary>
    public Sprite GroundSprite
    {
        get
        {
            return groundSprite;
        }

        set
        {
            groundSprite = value;
        }
    }

    /// <summary>
    /// Getter and Setter for item value.
    /// </summary>
    public double Value
    {
        get
        {
            return value;
        }

        set
        {
            this.value = value;
        }
    }

    /// <summary>
    /// Getter and Setter for item name.
    /// </summary>
    public string Name
    {
        get
        {
            return name;
        }

        set
        {
            name = value;
        }
    }

    /// <summary>
    /// Getter and Setter for item description.
    /// </summary>
    public string Description
    {
        get
        {
            return description;
        }

        set
        {
            description = value;
        }
    }

    /// <summary>
    /// Getter and Setter for item rarity.
    /// </summary>
    public ItemRarity Rarity
    {
        get
        {
            return rarity;
        }

        set
        {
            rarity = value;
        }
    }

    /// <summary>
    /// Getter and Setter for maximum item stacks.
    /// </summary>
    public int MaxStacks
    {
        get
        {
            return maxStacks;
        }

        set
        {
            if (value < 1)
            {
                maxStacks = 1;
            }
            else
            {
                maxStacks = value;
            }
        }
    }

    /// <summary>
    /// Getter and Setter for item quantity.
    /// </summary>
    public int Quantity
    {
        get
        {
            return quantity;
        }

        set
        {
            if (value > this.maxStacks)
            {
                quantity = maxStacks;
            }
            else if (value < 1)
            {
                quantity = 1;
            }
            else
            {
                quantity = value;
            }
        }
    }

    /// <summary>
    /// Getter for item ID.
    /// </summary>
    public int ItemID
    {
        get
        {
            return itemID;
        }
    }
    #endregion


}