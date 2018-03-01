using NUnit.Framework;
using NUnit.Framework.Internal;

/// <summary>
/// Tests the main holder object for the item inventory class.
/// </summary>
public class InventoryTest
{
    GameItem stackableItem;
    GameItem sameItem;
    GameItem diffItem;
    Inventory invent;

    /// <summary>
    /// Setup phase for constructing tests.
    /// </summary>
    [SetUp]
    public void Init()
    {
        stackableItem = new GameItem(null, null, "Generic Item", "I am an Item.", 10, GameItem.ItemRarity.COMMON, 5, 10, 2);
        diffItem = new GameItem(null, null, "Less Generic Item", "I am an Item.", 10, GameItem.ItemRarity.COMMON, 2, 6, 1);
        invent = new Inventory(16, 16);
    }

    /// <summary>
    /// Can items be added to the inventory?
    /// </summary>
    [Test]
    public void CanAddItems()
    {
        // Check if an item was added correctly.
        invent.AddItem(stackableItem);
        GameItem fetched = invent.GetItem(0, 0);
        Assert.IsNotNull(fetched);
        Assert.IsTrue(fetched.Equals(stackableItem));

        // Check if inventory structure behaves as expected.
        invent.AddItem(diffItem);
        GameItem fetched2 = invent.GetItem(1, 0);
        Assert.IsNotNull(fetched2);
        Assert.IsTrue(fetched2.Equals(diffItem));

        // Check if stacks correctly get updated.
        invent.AddItem(stackableItem);
        GameItem fetched3 = invent.GetItem(0, 0);
        Assert.IsTrue(fetched3.Quantity == 10);
        invent.AddItem(stackableItem);
        GameItem fetched4 = invent.GetItem(0, 0);
        Assert.IsTrue(fetched3.Quantity == 10);
        GameItem fetched5 = invent.GetItem(2, 0);
        Assert.IsNotNull(fetched5);
        Assert.IsTrue(fetched5.Equals(stackableItem));
        Assert.IsTrue(fetched5.Quantity == 5);
    }

    /// <summary>
    /// Can items be removed from the inventory?
    /// </summary>
    [Test]
    public void CanRemoveItems()
    {
        // Check if an item can be completely removed.
        invent.AddItem(stackableItem);
        invent.RemoveItem(0, 0, 5);
        Assert.IsNull(invent.GetItem(0, 0));

        // Check if an item can have its quantity removed.
        invent.AddItem(stackableItem);
        invent.RemoveItem(0, 0, 2);
        GameItem fetched1 = invent.GetItem(0, 0);
        Assert.IsNotNull(fetched1);
        Assert.IsTrue(fetched1.Quantity == 3);

        // Check if can remove a specific item.
        invent.RemoveItem(stackableItem, 3);
        Assert.IsNull(invent.GetItem(0, 0));
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