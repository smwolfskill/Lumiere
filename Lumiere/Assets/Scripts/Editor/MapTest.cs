using System.Collections;
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using System.ComponentModel;

/// <summary>
/// Test class for map functionality.
/// </summary>
public class MapTest
{
    GameMap map;
    GameObject gameMap;
    GameObject containerTestObject; //for testing class ContainerAttributes
    GenerateTiles tileGen;
    ContainerAttributes attrib;

    /// <summary>
    /// Setup steps for testing.
    /// </summary>
    [SetUp]
    public void Init()
    {
        map = new GameMap(10, 12);
        gameMap = new GameObject("testMap", typeof(GenerateTiles));
        containerTestObject = new GameObject("containerAttributesTestObject", typeof(ContainerAttributes));
        tileGen = gameMap.GetComponent<GenerateTiles>();
        attrib = containerTestObject.GetComponent<ContainerAttributes>();
    }

    /// <summary>
    /// Cleanup steps for testing.
    /// </summary>
    [TearDown]
    public void Cleanup()
    {
        // Nothing needed (yet).
    }

    /// <summary>
    /// Basic test for the underlying map structure, make sure it works.
    /// </summary>
    [Test]
    public void MapStructureTest()
    {
        // Game map should start out simple.
        Assert.IsNotNull(map);
        Assert.IsInstanceOf<WallTile>(map.GetTile(16, 16));
        Assert.IsInstanceOf<WallTile>(map.GetTile(70, 70));
        Assert.IsNull(map.GetTile(-10, -10));
        Assert.IsNull(map.GetTile(1000, 1000));
        Assert.AreEqual(10, map.GetWidth());
        Assert.AreEqual(12, map.GetHeight());
    }

    /// <summary>
    /// Tests attribute container class.
    /// </summary>
    [Test]
    public void ContainerAttributeTest()
    {
        Assert.IsNotNull(attrib);
        Assert.AreEqual(ContainerAttributes.ContainerType.EARTH, attrib.containerType);
        attrib.SetDimensions(0, 1, 2, 3);
        Assert.AreEqual(0, attrib.GetLeft());
        Assert.AreEqual(1, attrib.GetTop());
        Assert.AreEqual(2, attrib.GetWidth());
        Assert.AreEqual(3, attrib.GetHeight());
    }
    /*
    /// <summary>
    /// Theoretically tests the tile generation code. However, all of the code is private and the only information the generator returns is data for each tile.
    /// In other words we have to set the seed beforehand in order to test. Also, this code has not been fully incorperated yet and is here primarily as a spike.
    /// </summary>
    [Test]
    public void TileGeneratorTest()
    {
        // TODO: Modify this.
        Assert.IsNotNull(tileGen);
        Assert.AreEqual(tileGen.roomAttempts, 30);
        Assert.AreEqual(tileGen.pathAttempts, 30);
        Assert.AreEqual(tileGen.pathDirectionChangeLikelihood, 10);
        Assert.IsNotNull(tileGen.GetTile(15, 15));
    }
    */
}