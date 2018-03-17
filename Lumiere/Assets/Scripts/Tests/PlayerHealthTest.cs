using UnityEngine;
using UnityEngine.TestTools;
using NUnit.Framework;
using System.Collections;


public class PlayerHealthTest
{
    GameObject player;
    GameObject healthBarCanvas;
    GameObject canvasHealthBar;
    PlayerObject playerObject;
    GameObject playerHealthBar;
    EntityHealthManager playerHealthManager;

    /// <summary>
    /// Initialize the Player GameObject for use in all the tests.
    /// </summary>
    [SetUp]
    public void Init()
    {
        InitHealthBar();
        player = new GameObject("Player", typeof(EntityHealthManager));
        playerHealthManager = player.GetComponent<EntityHealthManager>();
        playerObject = new PlayerObject(player, 100.0f);
        playerHealthManager.entityObj = playerObject;
    }

    /// <summary>
    /// Initialize the health bar with the correct heirarchy.
    /// </summary>
    public void InitHealthBar()
    {
        healthBarCanvas = new GameObject ("UICanvas", typeof(RectTransform));
        canvasHealthBar = new GameObject("CanvasHealthBar", typeof(RectTransform));
        canvasHealthBar.transform.SetParent(healthBarCanvas.transform);
        playerHealthBar = new GameObject("PanelHealthBarFill", typeof(RectTransform), typeof(HealthBarManager));
        playerHealthBar.transform.SetParent(canvasHealthBar.transform);
    }

    /// <summary>
    /// Clean up everything after each test.
    /// </summary>
    [TearDown]
    public void Cleanup()
    {
        if (player != null)
        {
            GameObject.Destroy(player);
        }

        if (playerHealthBar != null)
        {
            GameObject.Destroy(playerHealthBar);
        }

        if (canvasHealthBar != null)
        {
            GameObject.Destroy(canvasHealthBar);
        }

        if(healthBarCanvas != null)
        {
            GameObject.Destroy(healthBarCanvas);
        }
    }

    /// <summary>
    /// Tests whether the max health is set correctly
    /// </summary>
    [Test]
    public void TestMaxHealth()
    {
        Assert.AreEqual (100.0f, playerObject.GetMaxHealth ());
    }

    /// <summary>
    /// Tests whether the player can take damage.
    /// </summary>
    [Test]
    public void TestDamagePlayer()
    {
        playerObject.InflictDamage (40.0f);
        Assert.AreEqual (60.0f, playerObject.GetCurrHealth());
    }

    /// <summary>
    /// Tests that the player dies if they get hit enough.
    /// </summary>
    [Test]
    public void TestPlayerDeath()
    {
        playerObject.InflictDamage (100.0f);
        Assert.IsTrue (playerObject.GetCurrHealth() <= 0.0f);
        Assert.IsTrue (playerObject.IsDead());
    }

    /// <summary>
    /// Tests that the player can heal.
    /// </summary>
    [Test]
    public void TestPlayerHeal()
    {
        playerObject.InflictDamage (99.0f);
        playerObject.Heal (9.0f);
        Assert.AreEqual (10.0f, playerObject.GetCurrHealth());
    }

    /// <summary>
    /// Tests that the player cannot overheal.
    /// </summary>
    [Test]
    public void TestPlayerOverHeal()
    {
        playerObject.Heal (1000.0f);
        Assert.AreEqual (playerObject.GetMaxHealth(), playerObject.GetCurrHealth());
    }

    /// <summary>
    /// Tests that the player cannot heal after death.
    /// </summary>
    [Test]
    public void TestPlayerHealAfterDeath()
    {
        playerObject.InflictDamage (100.0f);
        playerObject.Heal (1000.0f);
        Assert.IsTrue (playerObject.GetCurrHealth() <= 0.0f);
    }

    //TODO Add tests to check that the health bar's size has changed after damage has occured

}
