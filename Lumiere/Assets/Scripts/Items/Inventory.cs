using UnityEngine;
using UnityEditor;

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

        this.items = new GameItem[height, width];
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
        return this.items[y, x];
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
    public void RemoveItem(int x, int y, int quantity)
    {
        // TODO: Implement this function
        // It should work similar to the below:
        // Remember to implement idiot checks as neccessary.

        /* 
         * If item exists at location
         * -> Check if quantity of items can be removed.
         * ---> If so, remove x amount of items.
         * ---> If item slot is now empty (removed equal to or more than existing)
         * -----> Remove item from grid by setting this slot to null.
         * -> Item cannot be removed for some reason
         * ---> Handle this error (either by returning or something else).
         * If item does not exist a location (null location)
         * -> Handle this error somehow (either by returning or something else).
         */
        
    }

    /// <summary>
    /// Adds an item into the inventory
    /// </summary>
    /// <param name="item"></param>
    public void AddItem(GameItem item)
    {
        // TODO: Implement this function
        // It should work similar to the below:
        // Remember to implement idiot checks as neccessary.

        /* 
         * If item is stackable
         * -> Search array for existing stacks.
         * -> If existing stack is found.
         * ---> See if item can be added to stack.
         * ---> If not, keep looking.
         * ---> If so, increment stack size of existing item.
         * -> If existing stack is not found.
         * ---> Add item to top left most empty slot.
         * If item is not stackable
         * -> Add item to top left most empty slot.
         */
    }
    #endregion
}