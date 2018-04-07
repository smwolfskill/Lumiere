using UnityEngine;
using System.Collections.Generic;

/// <summary>
/// Representation of an inventory grid.
/// Note: Inventories cannot be resized once they are created (currently).
/// </summary>
public class Inventory
{
    public InventoryPanel uiPanel;

    protected int width;                // Width in blocks.
    protected int height;               // Height in blocks.

    protected GameItem[,] items;        // Array of items.
    protected EquipmentManager equips;  // Equipment management.

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
        this.equips = new EquipmentManager();
    }

    /// <summary>
    /// Copy constructor for an inventory.
    /// </summary>
    /// <param name="inv">Inventory to copy.</param>
    public Inventory(Inventory inv)
    {
        this.width = inv.width;
        this.height = inv.height;

        this.items = new GameItem[width, height];
        this.equips = new EquipmentManager();

        for (int i = 0; i < width; i++)
        {
            for (int j = 0; j < height; j++)
            {
                this.items[i, j] = inv.items[i, j];
            }
        }
    }

    /// <summary>
    /// Signal the UI to update, if present.
    /// </summary>
    public void UpdateUI()
    {
        if(uiPanel != null)
        {
            uiPanel.DrawInventory();
        }
    }

    /// <summary>
    /// Updates the UI quantity text at slot x and y.
    /// </summary>
    /// <param name="x">Location to be updated (x).</param>
    /// <param name="y">Location to be updated (y).</param>
    public void UpdateUIQuantityText(int x, int y)
    {
        if(uiPanel != null)
        {
            uiPanel.UpdateItemQuantityText(x, y);
        }
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

    /// <summary>
    /// Gets the equipment manager of the inventory.
    /// </summary>
    /// <returns>Equipment storage system for items.</returns>
    public EquipmentManager getEquipment()
    {
        return this.equips;
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
        if (currItem == null)   //set to UNSET_ITEM instead
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
    protected void SetItem(int x, int y, GameItem item)
    {
        this.items [x, y] = item;
        UpdateUI();
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
    public GameItem RemoveItem(int x, int y, int quantity)
    {
        GameItem itemInSlot = GetItem (x, y);
        GameItem removedItem = new GameItem(itemInSlot);
        if (!itemInSlot.SetYet())
        {
            return null;
        }
        else if (quantity > itemInSlot.Quantity)
        {
            return null;
        }
        else if (quantity == itemInSlot.Quantity)
        {
            itemInSlot.Quantity = 0;
            SetItem (x, y, GameItem.UNSET_ITEM);
            UpdateUI();
            return removedItem;
        } 
        else
        {
            removedItem.Quantity = quantity;
            itemInSlot.Quantity -= quantity;
            UpdateUIQuantityText(x, y);
            return removedItem;
        }
    }

    /// <summary>
    /// Removes however many of the item at the given grid location from the inventory.
    /// </summary>
    /// <param name="item">Item to remove. Includes quantity of items to remove.</param>
    /// <param name="quantity">(Optional)quantity of items to remove, if not set correctly inside the item.</param>
    /// <returns>If there are enough items in the inventory to remove this many items from the inventory.</returns>
    public bool RemoveItem(GameItem item, int quantity = -1)
    {
        if(!item.SetYet()) {    //cannot remove UNSET_ITEM from inventory.
            return false;
        }

        int currItemID = item.ItemID;
        int quantityLeft = quantity;
        if (quantity == -1)
        {
            quantityLeft = item.Quantity;
        }
        int currXPos = 0, currYPos = 0;

        List<int[]> itemsToReset = new List<int[]> (); //list of [x,y] positions of items to remove

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
                UpdateUIQuantityText(tmpItemLoc[0], tmpItemLoc[1]);
                quantityLeft = 0;
            }
            else     //add to list of slots to clear
            {
                itemsToReset.Add (tmpItemLoc);
                currXPos = tmpItemLoc[0] + 1;  //start the search **after** where we left off
                currYPos = tmpItemLoc[1];   //since the search iterates all X for a given Y, if currXPos 'overflows' it will start the next Y
                quantityLeft -= tmpItem.Quantity;
            }
        }

        //Remove specified instances of item found, whose quantities sum to the original quantityLeft to remove.
        foreach (int[] resetItemLoc in itemsToReset)
        {
            SetItem(resetItemLoc[0], resetItemLoc[1], GameItem.UNSET_ITEM);
        }
        if(itemsToReset.Count > 0)
        {
            UpdateUI();
        }
        return true;
    }

    /// <summary>
    /// Adds an item into the inventory
    /// </summary>
    /// <param name="item">The item to add.</param>
    /// <returns> Same item with quantity set to the amount not added, or null if everything was added. </returns>
    public GameItem AddItem(GameItem item)
    {
        if (!item.SetYet()) //cannot add UNSET_ITEM to inventory; 'empty' slots already represent it
        {
            return item;
        }

        GameItem item_cpy = new GameItem(item);
        int currXPos = 0, currYPos = 0;

        if (item_cpy.MaxStacks > 1) //stackable item
        {
            while (item_cpy.Quantity > 0)
            {
                //Try to find an item that is already in the inventory and whose stack is not full
                int[] existingItemInfo = FindItem (item_cpy.ItemID, false, currXPos, currYPos);

                //If such an item is found, add as many items as needed or possible to the existing item's quantity
                if (existingItemInfo != null)
                {
                    currXPos = existingItemInfo [0];
                    currYPos = existingItemInfo [1];
                    int maxAvailable = existingItemInfo [2];
                    GameItem foundItem = GetItem (currXPos, currYPos);
                    int amountToPut = Mathf.Min (maxAvailable, item_cpy.Quantity);
                    foundItem.Quantity = foundItem.Quantity + amountToPut;
                    item_cpy.Quantity -= amountToPut;
                    UpdateUIQuantityText(currXPos, currYPos);
                }
                else
                {
                    return AddItemToEmptySlot(item_cpy);
                }
            }
            return null;
        }
        else //non-stackable item
        {
            return AddItemToEmptySlot(item_cpy);
        }
    }
        
    /// <summary>
    /// Adds the item to the first available empty slot, disregarding stacking on other instances of the item.
    /// </summary>
    /// <returns><c>true</c>, if item was added to empty slot, <c>false</c> if inventory full.</returns>
    /// <param name="toAdd">To add.</param>
    protected GameItem AddItemToEmptySlot(GameItem toAdd)
    {
        int[] emptySlot = FindItem (GameItem.UNSET_ITEM.ItemID);
        if (emptySlot == null)
        {
            return toAdd;   //no empty slots found; inventory full!
        }
        SetItem (emptySlot [0], emptySlot [1], toAdd);
        UpdateUI();
        return null;
    }

    /// <summary>
    /// Finds an item already in the inventory.
    /// </summary>
    /// <param name="itemID">The ID of the item to search for.</param>
    /// <param name="includeFull">Whether or not to include items which are full stacks. If this is false, returns the amount of free space left in the item's stack.</param>
    /// <param name="startX">The X position from where to start searching.</param>
    /// <param name="startY">The Y position from where to start searching.</param>
    /// <returns>An integer array containing [xPos, yPos, (conditionally)emptySlots] of the item slot found.</returns>
    protected int[] FindItem(int itemID, bool includeFull = true, int startX = 0, int startY= 0)
    {
        for (int yPos = startY; yPos < this.height; yPos++)
        {
            for (int xPos = startX; xPos < this.width; xPos++)
            {
                GameItem currItem = GetItem (xPos, yPos);
                if (currItem.ItemID == itemID)
                {
                    if (!includeFull)   //don't include slots that are full to their max stack.
                    {
                        int freeSlots = currItem.MaxStacks - currItem.Quantity;
                        if (freeSlots > 0)
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
        return null; //not found

    }
    #endregion
}