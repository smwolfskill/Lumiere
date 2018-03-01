using NUnit.Framework;
using NUnit.Framework.Internal;

/// <summary>
/// Tests the Item class.
/// </summary>
public class ItemTest
{
    GameItem stackableItem;
    GameItem sameItem;
    GameItem diffItem;

    /// <summary>
    /// Setup phase for initilizing tests.
    /// </summary>
    [SetUp]
    public void Init()
    {
        stackableItem = new GameItem(null, null, "Generic Item", "I am an Item.", 10, GameItem.ItemRarity.COMMON, 5, 10, 2);
        sameItem = new GameItem(null, null, "Generic Item", "I am an Item.", 10, GameItem.ItemRarity.COMMON, 2, 10, 2);
        diffItem = new GameItem(null, null, "Less Generic Item", "I am an Item.", 10, GameItem.ItemRarity.COMMON, 2, 6, 1);
    }

    /// <summary>
    /// Does the item datatype store its data correctly, and can it be retrieved and set?
    /// </summary>
    [Test]
    public void ItemStoresData()
    {
        Assert.AreEqual(stackableItem.Value, 10);
        Assert.AreEqual(stackableItem.MaxStacks, 10);
        Assert.AreEqual(stackableItem.Quantity, 5);
        stackableItem.Quantity += 3;
        Assert.AreEqual(stackableItem.Quantity, 8);
    }

    /// <summary>
    /// Does the equals method work?
    /// </summary>
    [Test]
    public void ItemEqualsTest()
    {
        Assert.IsTrue(stackableItem.Equals(sameItem));
        Assert.IsFalse(stackableItem.Equals(diffItem));
    }
}