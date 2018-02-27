using UnityEngine;
using UnityEditor;
using System.Collections.Generic;

/// <summary>
/// Representation of an inventory grid.
/// Note: Inventories cannot be resized once they are created (currently).
/// </summary>
public class Inventory
{
    private int width;          // Width in blocks.
    private int height;         // Height in blocks.

    private GameItem[,] items;  // Array of items.

    /// <summary>
    /// Constructor for an inventory.
    /// </summary>
    /// <param name="nWidth">Width in tiles.</param>
    /// <param name="nHeight">Height in tiles.</param>
    public Inventory(int nWidth = 16, int nHeight = 16)
    {
        this.width = nWidth;
        this.height = nHeight;

        this.items = new GameItem[width, height];
    }

    #region Getters And Setters
    /// <summary>
    /// Fetches the width of the inventory.
    /// </summary>
    /// <returns>Width in blocks.</returns>
    public int GetWidth()
    {
        return this.width;
    }

    /// <summary>
    /// Fetches the height of the inventory.
    /// </summary>
    /// <returns>Width in blocks.</returns>
    public int GetHeight()
    {
        return this.height;
    }

    /// <summary>
    /// Gets the full array of items.
    /// </summary>
    /// <returns>The full array of items.</returns>
    public GameItem[,] GetItems()
    {
        return this.items;
    }
    #endregion

    #region Inventory Management
    /// <summary>
    /// Fetches information in regards to a specific item in the grid.
    /// </summary>
    /// <param name="x">X coordinate in grid of item.</param>
    /// <param name="y">Y coordinate in grid of item.</param>
    /// <returns>The item at the given point.</returns>
    public GameItem GetItem(int x, int y)
    {
        GameItem currItem = this.items[x, y];
        if (currItem == null)
        {
            this.items [x, y] = GameItem.UNSET_ITEM;
            currItem = this.items[x, y];
        }
        return currItem;
    }

    /// <summary>
    /// Fetches information in regards to a specific item in the grid.
    /// </summary>
    /// <param name="x">X coordinate in grid of item.</param>
    /// <param name="y">Y coordinate in grid of item.</param>
    /// <param name="item">The item to set this grid item to.</param>
    private void SetItem(int x, int y, GameItem item)
    {
        this.items [x, y] = item;
    }

    /// <summary>
    /// Moves an item from one spot on the grid to another, player sorting. (IT4 likely)
    /// </summary>
    /// <param name="itemX">Current location (x) of item stack.</param>
    /// <param name="itemY">Current location (y) of item stack.</param>
    /// <param name="newX">New location (x) for item stack.</param>
    /// <param name="newY">New location (y) for item stack.</param>
    public void MoveStack(int itemX, int itemY, int newX, int newY)
    {
        // TODO: Implement this function
        // It should work similar to the below:
        // Remember to implement idiot checks as neccessary.

        /*
         * If an item exists at newX and newY.
         * -> See if item can be stacked there.
         * ---> If the item is a different one, move that stack to the original itemX and Y.
         * ---> If the item is the same and can be stacked, add to the new location's item stack, remove old item.
         * ---> If the item is the same and cannot be fully stacked, set new pile to max, keep old stack at a smaller quantity.
         * If an item does not exist at newX and newY.
         * -> Simply move the stack.
         */
    }

    /// <summary>
    /// Moves an item from one spot on the grid to another, player sorting. (IT4 likely)
    /// </summary>
    /// <param name="itemX">Current location (x) of item stack.</param>
    /// <param name="itemY">Current location (y) of item stack.</param>
    /// <param name="newX">New location (x) for item stack.</param>
    /// <param name="newY">New location (y) for item stack.</param>
    /// <param name="newQuantity">Quantity of the new stack.</param>
    public void SplitStack(int itemX, int itemY, int newX, int newY, int newQuantity)
    {
        // TODO: Implement this function
        // It should work similar to the below:
        // Remember to implement idiot checks as neccessary.

        /*
         * If newQuantity > sizeOfOld 
         * -> Simply implement it identically to MoveStack.
         * Otherwise          
         * -> Create new item based on old of size newQuantity.
         * -> Follow MoveStack algorithm for new stack.
         * -> Reduce quantity of old stack.
         */
    }
        
    /// <summary>
    /// Removes however many of the item at the given grid location from the inventory.
    /// </summary>
    /// <param name="x">Location of the item (x).</param>
    /// <param name="y">Location of the item (y).</param>
    /// <param name="quantity">Quantity of items to remove.</param>
    public bool RemoveItem(int x, int y, int quantity)
    {
        GameItem itemInSlot = GetItem (x, y);
        if (!itemInSlot.SetYet())
        {
            return false;
        }

        if (quantity > itemInSlot.Quantity)
        {
            return false;
        }

        if (quantity == itemInSlot.Quantity)
        {
            SetItem (x, y, GameItem.UNSET_ITEM);
            return true;
        }

        if (quantity < itemInSlot.Quantity)
        {
            itemInSlot.Quantity = itemInSlot.Quantity - quantity;
            return true;
        }
        return false;
    }

    /// <summary>
    /// Removes however many of the item at the given grid location from the inventory.
    /// </summary>
    /// <param name="item">Item to remove. Includes quantity of items to remove.</param>
    /// <param name="quantity">(Optional)quantity of items to remove, if not set correctly inside the item.</param>
    /// <returns>If there are enough items in the inventory to remove this many items from the inventory.</returns>
    public bool RemoveItem(GameItem item, int quantity = -1)
    {
        int currItemID = item.ItemID;
        if (currItemID == GameItem.UNSET_ITEM.ItemID)
        {
            return false;
        }

        int quantityLeft = quantity;
        if (quantity == -1)
        {
            quantityLeft = item.Quantity;
        }
        int currXPos = 0, currYPos = 0;

        List<int[]> itemsToReset = new List<int[]> ();

        while (quantityLeft > 0)
        {
            int[] tmpItemLoc = FindItem (currItemID, true, currXPos, currYPos);
            if (tmpItemLoc == null)
            {
                return false;
            }
            GameItem tmpItem = GetItem(tmpItemLoc [0], tmpItemLoc [1]);
            if (tmpItem.Quantity > quantityLeft)
            {
                tmpItem.Quantity -= quantityLeft;
                quantityLeft = 0;
            }
            else if (tmpItem.Quantity == quantityLeft)
            {
                tmpItem = GameItem.UNSET_ITEM;
                quantityLeft = 0;
            }
            else
            {
                itemsToReset.Add (tmpItemLoc);
                quantityLeft -= tmpItem.Quantity;
            }
        }
        foreach (int[] resetItemLoc in itemsToReset)
        {
            SetItem(resetItemLoc[0], resetItemLoc[1], GameItem.UNSET_ITEM);
        }
        return true;
    }

    /// <summary>
    /// Adds an item into the inventory
    /// </summary>
    /// <param name="item">The item to add.</param>
    public bool AddItem(GameItem item)
    {

        int currItemID = item.ItemID;
        if (currItemID == GameItem.UNSET_ITEM.ItemID)
        {
            return false;
        }

        int quantityLeft = item.Quantity;
        int currXPos = 0, currYPos = 0;

        if (item.MaxStacks > 1)
        {
            while (quantityLeft > 0)
            {
                //Try to find an item that is already in the inventory and whose stack is not full
                int[] existingItemInfo = FindItem (currItemID, false, currXPos, currYPos);

                //If such an item is found, add as many items as needed or possible to the existing item's quantity
                if (existingItemInfo != null)
                {
                    currXPos = existingItemInfo [0];
                    currYPos = existingItemInfo [1];
                    int maxAvailable = existingItemInfo [2];
                    GameItem foundItem = GetItem (currXPos, currYPos);
                    int amountToPut = Mathf.Min (maxAvailable, quantityLeft);
                    foundItem.Quantity = foundItem.Quantity + amountToPut;
                    quantityLeft -= amountToPut;
                }
                else
                {
                    int[] emptySlot = FindItem (GameItem.UNSET_ITEM.ItemID);
                    if (emptySlot == null)
                    {
                        return false;
                    }
                    SetItem (emptySlot [0], emptySlot [1], item);
                }
            }
        }
        else
        {
            int[] emptySlot = FindItem (GameItem.UNSET_ITEM.ItemID);
            if (emptySlot == null)
            {
                return false;
            }
            SetItem (emptySlot [0], emptySlot [1], item);
        }
        return true;
    }


    /// <summary>
    /// Finds an item already in the inventory
    /// </summary>
    /// <param name="itemID">The ID of the item to search for.</param>
    /// <param name="includeFull">Whether or not to include items which are full stacks. If this is false, returns the amount of free space left in the item's stack.</param>
    /// <param name="startX">The X position from where to start searching.</param>
    /// <param name="startY">The Y position from where to start searching.</param>
    /// <returns>An integer array containing [xPos, yPos, (conditionally)emptySlots] of the item slot found.</returns>
    private int[] FindItem(int itemID, bool includeFull = true, int startX = 0, int startY= 0)
    {
        for (int xPos = startX; xPos < this.width; xPos++)
        {
            for (int yPos = startY; yPos < this.height; yPos++)
            {
                GameItem currItem = GetItem (xPos, yPos);
                if (currItem.ItemID == itemID)
                {
                    if (!includeFull)
                    {
                        int freeSlots = currItem.MaxStacks - currItem.Quantity;
                        if (freeSlots != 0)
                        {
                            int[] itemInfo = { xPos, yPos, freeSlots };
                            return itemInfo;
                        }
                    }
                    else
                    {
                        int[] itemInfo = { xPos, yPos };
                        return itemInfo;
                    }
                }
            }
        }
        return null;

    }
    #endregion
}