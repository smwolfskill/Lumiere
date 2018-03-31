using UnityEngine;
using System.Collections.Generic;
using System.Collections;
using System;


/// <summary>
/// Representation of a generic item in the game.
/// </summary>
[System.Serializable]
public class GameItem
{
    public static GameItem UNSET_ITEM = new GameItem ();

    [SerializeField]
    protected Sprite guiSprite;       // Item in GUI.
    protected Texture2D guiTexture;   // Texture for item in GUI.

    [SerializeField]
    protected Sprite groundSprite;    // Item on ground.

    [SerializeField]
    protected double value;           // Arbitrary value of item, to be used for trading

    [SerializeField]
    protected string name;            // Item name.

    [SerializeField]
    protected string description;     // Item description.

    [SerializeField]
    protected ItemRarity rarity;      // Item rarity.

    [SerializeField]
    protected int maxStacks;          // Maximum stack quantity.

    [SerializeField]
    protected int quantity;           // Amount of items.

    [SerializeField]
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
    /// Create a new copy of an item.
    /// </summary>
    /// <param name="copy">Copy.</param>
    public GameItem(GameItem copy)
    {
        this.FillData(copy);
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
    private void FillData(Sprite gui = null, Sprite ground = null, string newName = "Unknown", string newDesc = "ERROR: An unknown item.", double val = 0.0, ItemRarity rareness = ItemRarity.COMMON, int itemQuantity = 1, int newMaxStack = 1, int itemID = -1, string useAction = null)
    {
        this.guiSprite = gui;
        this.groundSprite = ground;
        this.name = newName;
        this.description = newDesc;
        this.rarity = rareness;
        this.value = val;
        this.itemID = itemID;

        SetGuiTexture();

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
        else if (itemQuantity < 0)
        {
            this.quantity = 0;
        }
        else
        {
            this.quantity = itemQuantity;
        }
    }

    /// <summary>
    /// Fills out the class data as an exact copy of another GameItem.
    /// </summary>
    /// <param name="copy">Copy.</param>
    private void FillData(GameItem copy)
    {
        FillData(null, copy.groundSprite, copy.name, 
                 copy.description, copy.value, copy.rarity, 
                 copy.quantity, copy.maxStacks, copy.itemID);
        //Copy the gui sprites and textures here to avoid duplicating texture generation.
        this.guiSprite = copy.guiSprite;
        this.guiTexture = copy.guiTexture;
    }

    /// <summary>
    /// An equals method for comparing this item to other items. Auto generated.
    /// </summary>
    /// <param name="obj">Object being compared</param>
    /// <returns>True if the item is equal and False otherwise.</returns>
    public override bool Equals(object obj)
    {
        var item = obj as GameItem;
        bool guiSpritesEqual = false;
        bool groundSpritesEqual = false;
        if (guiSprite == null && item.guiSprite == null)
        {
            guiSpritesEqual = true;
        }
        else
        {
            guiSpritesEqual = 
                EqualityComparer<Sprite>.Default.Equals (guiSprite, item.guiSprite);
        }

        if (groundSprite == null && item.groundSprite == null)
        {
            groundSpritesEqual = true;
        }
        else
        {
            groundSpritesEqual = 
                EqualityComparer<Sprite>.Default.Equals (groundSprite, item.groundSprite);
        }

        return item != null &&
               guiSpritesEqual &&
               groundSpritesEqual &&
               value == item.value &&
               name == item.name &&
               description == item.description &&
               rarity == item.rarity &&
               maxStacks == item.maxStacks &&
               itemID == item.itemID;
    }

    /// <summary>
    /// Determine if this item is not the UNSET_ITEM.
    /// </summary>
    /// <returns><c>true</c>, if set.<c>false</c> if this GameItem is UNSET_ITEM.</returns>
    public bool SetYet()
    {
        if(itemID == UNSET_ITEM.itemID)
        {
            return false;
        }
        return true;
    }

    /// <summary>
    /// Create and set the GUI Texture based on guiSprite.
    /// </summary>
    protected void SetGuiTexture()
    {
        if (guiSprite != null)
        {
            guiTexture = TextureFromSprite(guiSprite);
        }
        else 
        {
            guiTexture = null;
        }
    }

    /// <summary>
    /// Returns the rarity color of the given item.
    /// </summary>
    /// <param name="item">Item to examine.</param>
    public Color RarityColor()
    {
        float opaqueness = 1.0f;
        Color commonColor = new Color(1f, 1f, 1f, opaqueness);
        switch (rarity)
        {
        case ItemRarity.COMMON:
            return commonColor;
        case ItemRarity.UNCOMMON:
            return new Color(167f / 255f, 1f, 215f / 255f, opaqueness);
        case ItemRarity.RARE:
            return new Color(21f / 255f, 1f, 231f / 255f, opaqueness);
        case ItemRarity.EPIC:
            return new Color(51f / 255f, 60f / 255f, 1f, opaqueness);
        case ItemRarity.LEGENDARY:
            return new Color(1f, 11f / 255f, 253f / 255f, opaqueness);
        default:
            Debug.Log("GameItem.RarityColor: Please add another color for rarity '" + rarity.ToString("G") + "'");
            return commonColor;
        }
    }

    /// <summary>
    /// Creates a game object representing the GameItem in the physical world as a dropped item.
    /// </summary>
    /// <returns>The game object.</returns>
    /// <param name="position">Position to spawn the dropped item at.</param>
    public GameObject CreateGameObject(Vector3 position)
    {
        GameObject droppedItem = (GameObject) GameObject.Instantiate(Resources.Load<GameObject>("Prefabs/Item"));
        droppedItem.transform.position = position;
        ItemManager itemManager = droppedItem.GetComponent<ItemManager>();
        itemManager.item = this;
        return droppedItem;
    }

    /// <summary>
    /// Yanks a texture representing the image of a sprite from the sprite
    /// </summary>
    /// <param name="s">The sprite.</param>
    protected Texture2D TextureFromSprite(Sprite sprite)
    {
        if (sprite.rect.width != sprite.texture.width)
        {
            // Crop the texture
            Texture2D t = new Texture2D((int)sprite.textureRect.width, (int)sprite.textureRect.height);
            t.SetPixels(sprite.texture.GetPixels((int)sprite.textureRect.x,
                (int)sprite.textureRect.y,
                (int)sprite.textureRect.width,
                (int)sprite.textureRect.height));
            t.Apply();

            return t;
        }
        else
        {
            // We really don't need to do any processing here
            return sprite.texture;
        }
    }

    public string TooltipText()
    {
        string nl = Environment.NewLine;
        return "<i>Name:</i> " + name + nl
             + "<i>Description:</i> " + description;
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
            SetGuiTexture();
        }
    }

    public Texture2D GuiTexture
    {
        get
        {
            return guiTexture;
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
            else if (value < 0)
            {
                quantity = 0;
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