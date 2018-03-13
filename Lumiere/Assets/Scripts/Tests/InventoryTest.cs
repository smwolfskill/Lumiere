using NUnit.Framework;
using NUnit.Framework.Internal;
using System.Diagnostics;

/// <summary>
/// Tests the main holder object for the item inventory class.
/// </summary>
public class InventoryTest
{
    GameItem stackableItem;
    GameItem stackableItem2; //copy of stackableItem
    GameItem sameItem;
    GameItem diffItem;
    Inventory invent;

    /// <summary>
    /// Setup phase for constructing tests. Called before each test.
    /// </summary>
    [SetUp]
    public void Init()
    {
        stackableItem = new GameItem(null, null, "Generic Item", "I am an Item.", 10, GameItem.ItemRarity.COMMON, 5, 10, 2);
        stackableItem2 = new GameItem(stackableItem);
        diffItem = new GameItem(null, null, "Less Generic Item", "I am an Item.", 10, GameItem.ItemRarity.COMMON, 2, 6, 1);
        invent = new Inventory(16, 16);
    }

    /// <summary>
    /// Can items be added to the inventory?
    /// </summary>
    [Test]
    public void CanAddItems()
    {
        // Make sure inventory starts out empty
        GameItem beginning = invent.GetItem(0, 0);
        Assert.AreEqual(GameItem.UNSET_ITEM, beginning);

        // Check if an item was added correctly.
        GameItem remaining = invent.AddItem(stackableItem);
        Assert.IsNull(remaining);
        GameItem fetched = invent.GetItem(0, 0);
        Assert.IsTrue(fetched.Equals(stackableItem));
        Assert.AreEqual(5, fetched.Quantity);

        // Check if inventory structure behaves as expected.
        GameItem remaining2 = invent.AddItem(diffItem);
        Assert.IsNull(remaining2);
        GameItem fetched2 = invent.GetItem(1, 0);
        Assert.AreNotEqual(GameItem.UNSET_ITEM, fetched2);
        Assert.IsTrue(fetched2.Equals(diffItem));

        // Check if stacks correctly get updated.
        //1. Fill stackableItem slot to its max stack.
        int q = stackableItem.Quantity;
        GameItem remaining3 = invent.AddItem(stackableItem);
        Assert.IsNull(remaining3);
        GameItem fetched3 = invent.GetItem(0, 0);
        Assert.AreEqual(stackableItem.MaxStacks, fetched3.Quantity);
        //Note that stackableItem.Quantity has been updated as well since the inventory stores it only as a reference.

        //2. Add a new copy of stackableItem which should be in its own slot.
        GameItem remaining4 = invent.AddItem(stackableItem2);
        Assert.IsNull(remaining4);
        GameItem fetched4 = invent.GetItem(0, 0);
        Assert.AreEqual(stackableItem.MaxStacks, fetched4.Quantity); //original copy unchanged
        GameItem fetched5 = invent.GetItem(2, 0);
        Assert.AreNotEqual(GameItem.UNSET_ITEM, fetched5);
        Assert.IsTrue(fetched5.Equals(stackableItem2));
        Assert.AreEqual(5, stackableItem2.Quantity);
        Assert.AreEqual(5, fetched5.Quantity); //new copy still has correct quantity
    }

    /// <summary>
    /// Tests that Inventory can be filled to max capacity and return the exact quantity of items that cannot be added.
    /// </summary>
    [Test]
    public void TestInventoryFull()
    {
        Inventory smallInv = new Inventory(1, 1);
        stackableItem.Quantity = 8;
        stackableItem.MaxStacks = 10;
        GameItem remaining = smallInv.AddItem(stackableItem);
        Assert.IsNull(remaining);

        //1. Try adding again to fill up inventory completely, and items that couldn't fit should be returned
        remaining = smallInv.AddItem(stackableItem);
        int expectedLeft = stackableItem.Quantity - (stackableItem.MaxStacks - stackableItem.Quantity);
        Assert.IsNotNull(remaining);
        Assert.AreEqual(expectedLeft, remaining.Quantity);
        GameItem fullItem = smallInv.GetItem(0, 0);

        //Ensure inventory is full
        Assert.IsNotNull(fullItem);
        Assert.AreEqual(stackableItem.MaxStacks, fullItem.Quantity);
    }

    /// <summary>
    /// Can items be removed from the inventory at specified slot indices?
    /// (Relies on AddItem functionality)
    /// </summary>
    [Test]
    public void CanRemoveItemsOnSlot()
    {
        int baseQuantity = stackableItem.Quantity;

        // Check if an item can be completely removed.
        invent.AddItem(stackableItem);
        GameItem removed = invent.RemoveItem(0, 0, baseQuantity);
        Assert.IsNotNull(removed);
        Assert.IsTrue(stackableItem.Equals(removed));
        Assert.AreEqual(GameItem.UNSET_ITEM, invent.GetItem(0, 0));

        // Check if an item can have part of its quantity removed.
        invent.AddItem(stackableItem2);
        int toRemove = baseQuantity - 2;
        removed = invent.RemoveItem(0, 0, toRemove);
        Assert.IsNotNull(removed);
        Assert.AreEqual(toRemove, removed.Quantity);
        GameItem fetched1 = invent.GetItem(0, 0);
        int expectedQuantity = baseQuantity - toRemove;
        Assert.AreNotEqual(GameItem.UNSET_ITEM, fetched1);
        Assert.AreEqual(expectedQuantity, fetched1.Quantity);

        // Check if failure for attempting to remove more than is present. No items should be removed
        removed = invent.RemoveItem(0, 0, baseQuantity + 1);
        Assert.IsNull(removed);
        GameItem fetched2 = invent.GetItem(0, 0);
        Assert.AreNotEqual(GameItem.UNSET_ITEM, fetched2);
        Assert.AreEqual(expectedQuantity, fetched2.Quantity); //nothing removed
    }

    /// <summary>
    /// Can items be removed from inventory without slot indices specified?
    /// </summary>
    /// <returns><c>true</c> if this instance can remove items; otherwise, <c>false</c>.</returns>
    [Test]
    public void CanRemoveItems()
    {
        int baseQuantity = stackableItem.Quantity;

        // Check if an item can be completely removed.
        invent.AddItem(stackableItem);
        bool removed = invent.RemoveItem(stackableItem, baseQuantity);
        Assert.IsTrue(removed);
        Assert.AreEqual(GameItem.UNSET_ITEM, invent.GetItem(0, 0));

        // Check if an item can have part of its quantity removed.
        invent.AddItem(stackableItem2);
        int toRemove = baseQuantity - 2;
        removed = invent.RemoveItem(stackableItem2, toRemove);
        Assert.IsTrue(removed);
        GameItem fetched1 = invent.GetItem(0, 0);
        int expectedQuantity = baseQuantity - toRemove;
        Assert.AreNotEqual(GameItem.UNSET_ITEM, fetched1);
        Assert.AreEqual(expectedQuantity, fetched1.Quantity);

        // Check if failure for attempting to remove more than is present. No items should be removed
        removed = invent.RemoveItem(stackableItem2, baseQuantity + 1);
        Assert.IsFalse(removed);
        GameItem fetched2 = invent.GetItem(0, 0);
        Assert.AreNotEqual(GameItem.UNSET_ITEM, fetched2);
        Assert.AreEqual(expectedQuantity, fetched2.Quantity); //nothing removed
    }

    /*
    public void CanMoveStacks()
    {

    }

    public void CanSplitStacks()
    {

    }
     */
}