using UnityEngine;
using UnityEngine.TestTools;
using UnityEngine.UI;
using NUnit.Framework;
using System.Collections;


public class PlayerHealthTest
{
    GameObject player;
    GameObject camera;
    GameObject UICanvas;
    GameObject canvasHealthBar;
    GameObject invPanel;
    GameObject hotbarPanel;
    GameObject equipmentPanel;
    Animator anim;
    PlayerObject playerObject;
    GameObject playerHealthBar;
    EntityHealthManager playerHealthManager;
    Player _player;
    EquipmentManager equipmentManager;

    /// <summary>
    /// Initialize the Player GameObject for use in all the tests.
    /// </summary>
    [SetUp]
    public void Init()
    {
        UICanvas = new GameObject("UICanvas",
                                  typeof(RectTransform),
                                  typeof(UIBehavior))
        {
            tag = "UICanvas"
        };
        InitHealthBar();
        InitEquipmentPanels();
        _player = new Player();
        camera = new GameObject("Main Camera",
                                typeof(Camera),
                                typeof(CameraFollow))
        {
            tag = "MainCamera"
        };
        camera.GetComponent<Camera>().enabled = true;
        invPanel = new GameObject("Inventory Panel",
                                  typeof(InventoryPanel),
                                  typeof(GridLayoutGroup))
        {
            tag = "InventoryPanel"
        };
        invPanel.GetComponent<InventoryPanel>().entity = _player;

        UICanvas.GetComponent<UIBehavior>().inventoryPanel = invPanel;

        player = new GameObject("Player", typeof(EntityHealthManager));
        anim = player.AddComponent<Animator>();
        playerHealthManager = player.GetComponent<EntityHealthManager>();
        playerObject = new PlayerObject(player, 100.0f);
        playerHealthManager.entityObj = playerObject;
    }

    /// <summary>
    /// Initialize the health bar with the correct heirarchy.
    /// </summary>
    public void InitHealthBar()
    {

        canvasHealthBar = new GameObject("CanvasHealthBar",
                                         typeof(RectTransform));
        canvasHealthBar.transform.SetParent(UICanvas.transform);
        playerHealthBar = new GameObject("PanelHealthBarFill",
                                         typeof(RectTransform),
                                         typeof(HealthBarManager));
        playerHealthBar.transform.SetParent(canvasHealthBar.transform);
    }

    /// <summary>
    /// Initialize the panels used by the EquipmentManager with the correct
    /// heirarchy.
    /// </summary>
    public void InitEquipmentPanels()
    {
        equipmentManager = new EquipmentManager();
        hotbarPanel = new GameObject("HotbarPanel",
                                     typeof(HotbarPanel),
                                     typeof(GridLayoutGroup))
        {
            tag = "HotbarPanel"
        };
        hotbarPanel.transform.SetParent(UICanvas.transform);
        hotbarPanel.GetComponent<HotbarPanel>().SetEquipmentManager(
            equipmentManager);

        equipmentPanel = new GameObject("EquipmentPanel",
                                        typeof(EquipmentPanel),
                                        typeof(GridLayoutGroup))
        {
            tag = "EquipmentPanel"
        };
        equipmentPanel.transform.SetParent(UICanvas.transform);
        equipmentPanel.GetComponent<EquipmentPanel>().Manager =
            equipmentManager;
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

        if (UICanvas != null)
        {
            GameObject.Destroy(UICanvas);
        }
    }

    /// <summary>
    /// Tests whether the max health is set correctly
    /// </summary>
    [Test]
    public void TestMaxHealth()
    {
        Assert.AreEqual(100.0f, playerObject.GetMaxHealth());
    }

    /// <summary>
    /// Tests whether the player can take damage.
    /// </summary>
    [Test]
    public void TestDamagePlayer()
    {
        playerObject.InflictDamage(40.0f);
        Assert.AreEqual(60.0f, playerObject.GetCurrHealth());
    }

    /// <summary>
    /// Tests that the player dies if they get hit enough.
    /// </summary>
    [Test]
    public void TestPlayerDeath()
    {
        playerObject.InflictDamage(100.0f);
        Assert.IsTrue(playerObject.GetCurrHealth() <= 0.0f);
        Assert.IsTrue(playerObject.IsDead());
    }

    /// <summary>
    /// Tests that the player can heal.
    /// </summary>
    [Test]
    public void TestPlayerHeal()
    {
        playerObject.InflictDamage(99.0f);
        playerObject.Heal(9.0f);
        Assert.AreEqual(10.0f, playerObject.GetCurrHealth());
    }

    /// <summary>
    /// Tests that the player cannot overheal.
    /// </summary>
    [Test]
    public void TestPlayerOverHeal()
    {
        playerObject.Heal(1000.0f);
        Assert.AreEqual(playerObject.GetMaxHealth(), playerObject.GetCurrHealth());
    }

    /// <summary>
    /// Tests that the player cannot heal after death.
    /// </summary>
    [Test]
    public void TestPlayerHealAfterDeath()
    {
        playerObject.InflictDamage(100.0f);
        playerObject.Heal(1000.0f);
        Assert.IsTrue(playerObject.GetCurrHealth() <= 0.0f);
    }

    // TODO Add tests to check that the health bar's size has changed after
    // damage has occured

}
