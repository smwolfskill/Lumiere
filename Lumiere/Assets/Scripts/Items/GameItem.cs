using UnityEngine;
using UnityEditor;

/// <summary>
/// Representation of a generic item in the game.
/// </summary>
public class GameItem
{
    private Sprite guiSprite;       // Item in GUI.
    private Sprite groundSprite;    // Item on ground.

    private double value;           // Arbitrary value of item, to be used for trading
    private string name;            // Item name.
    private string description;     // Item description.
    private ItemRarity rarity;      // Item rarity.
    private int maxStacks;          // Maximum stack quantity.
    private int quantity;           // Amount of items.

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
    public GameItem(Sprite gui, Sprite ground, string newName, string newDesc, double val, ItemRarity rareness, int itemQuantity = 1, int newMaxStack = 1)
    {
        this.FillData(gui, ground, newName, newDesc, val, rareness, itemQuantity, newMaxStack);
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
    private void FillData(Sprite gui = null, Sprite ground = null, string newName = "Unknown", string newDesc = "ERROR: An unknown item.", double val = 0.0, ItemRarity rareness = ItemRarity.COMMON, int itemQuantity = 1, int newMaxStack = 1)
    {
        this.guiSprite = gui;
        this.groundSprite = ground;
        this.name = newName;
        this.description = newDesc;
        this.rarity = rareness;
        this.value = val;

        // Idiot proofing.
        if (newMaxStack < 1)
            this.maxStacks = 1;
        else
            this.maxStacks = newMaxStack;

        if (itemQuantity > newMaxStack)
            this.quantity = newMaxStack;
        else if (itemQuantity < 1)
            this.quantity = 1;
        else
            this.quantity = itemQuantity;
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
                maxStacks = 1;
            else
                maxStacks = value;
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
                quantity = maxStacks;
            else if (value < 1)
                quantity = 1;
            else
                quantity = value;
        }
    }
    #endregion
}