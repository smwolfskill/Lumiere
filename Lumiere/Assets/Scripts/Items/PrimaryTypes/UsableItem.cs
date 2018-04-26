using UnityEngine;

// TODO: Verify this works with demo systems.

/// <summary>
/// Representation of an item that can be used in some fashion.
/// </summary>
public class UsableItem : GameItem
{
    // Represents action this item performs.
    protected ItemAction action;

    // TODO: Make this private or protected within Unity's system mechanics to preserve OOP.
    [SerializeField]
    public string useAction;         //name of corresponding action asset to load in Resources/Actions/ItemActions/.

    /// <summary>
    /// Base constructor for a usable item.
    /// </summary>
    /// <param name="useAction">Optional use action that describes on use functionality.</param>
    public UsableItem(string useAction = null) : base()
    {
        InitAction(-1, useAction);
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
    /// <param name="useAction">Optional use action that describes on use functionality.</param>
    public UsableItem(Sprite gui, Sprite ground, string newName, string newDesc, double val, ItemRarity rareness, int itemQuantity = 1, int newMaxStack = 1, int itemID = -1, string useAction = null) : base(gui, ground, newName, newDesc, val, rareness, itemQuantity, newMaxStack, itemID)
    {
        InitAction(itemID, useAction);
    }

    override public GameItem clone()
    {
        return new UsableItem(
            this.guiSprite,
            this.groundSprite,
            this.name,
            this.description,
            this.value,
            this.rarity,
            this.quantity,
            this.maxStacks,
            this.itemID,
            this.useAction);
    }

    /// <summary>
    /// Initilizes the item data for a usable item.
    /// </summary>
    /// <param name="itemID">The ID of the item.</param>
    /// <param name="useAction">Optional use action that describes on use functionality.</param>
    private void InitAction(int itemID, string useAction)
    {
        this.useAction = useAction;
        if (useAction != null && useAction != "")
        {
            this.action = Resources.Load<ItemAction>("Actions/ItemActions/" + useAction);
            this.action.itemID = itemID;
        }
    }

    /// <summary>
    /// A validation function for items uses. Required for front-end behavior.
    /// </summary>
    /// <param name="obj">Object to validate the action on.</param>
    /// <returns>True if successful, False otherwise.</returns>
    public bool ValidateUse(GameObject obj)
    {
        return action != null && action.Validate(obj);
    }

    /// <summary>
    /// A use function for the item. Required for front-end behavior.
    /// </summary>
    /// <param name="obj">Object to perform the action on.</param>
    /// <returns>True if successful, False otherwise.</returns>
    public bool Use(GameObject obj)
    {
        return action.Execute(obj);
    }
}