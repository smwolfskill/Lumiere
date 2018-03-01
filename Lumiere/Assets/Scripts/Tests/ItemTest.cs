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
        Assert.AreEqual(10, stackableItem.Value);
        Assert.AreEqual(10, stackableItem.MaxStacks);
        Assert.AreEqual(5, stackableItem.Quantity);
        Assert.AreEqual(2, stackableItem.ItemID);
        stackableItem.Quantity += 3;
        Assert.AreEqual(8, stackableItem.Quantity);
    }

    /// <summary>
    /// Does the equals method work?
    /// </summary>
    [Test]
    public void ItemEqualsTest()
    {
        Assert.IsTrue(stackableItem.Equals(sameItem));
        Assert.IsFalse(stackableItem.Equals(diffItem));
        Assert.IsFalse(stackableItem.Equals(GameItem.UNSET_ITEM));
    }

    /// <summary>
    /// Test if SetYet functionality works, which returns false if a GameItem has the same ItemID as the UNSET_ITEM.
    /// </summary>
    [Test]
    public void SetYetTest()
    {
        //1. Assert UNSET_ITEM is not Set Yet.
        Assert.IsFalse(GameItem.UNSET_ITEM.SetYet());

        //2. Assert newly created default GameItem is not Set Yet.
        GameItem unset = new GameItem();
        Assert.IsFalse(unset.SetYet());

        //3. Assert custom created GameItem is Set Yet.
        Assert.IsTrue(stackableItem.SetYet());
    }

    /// <summary>
    /// Test if copying a GameItem works by preserving equality.
    /// </summary>
    [Test]
    public void ItemCopyTest()
    {
        GameItem stackableItem_copy = new GameItem(stackableItem);
        Assert.IsTrue(stackableItem_copy.Equals(stackableItem));
    }
}