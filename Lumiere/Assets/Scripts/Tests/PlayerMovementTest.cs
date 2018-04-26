using UnityEngine;
using UnityEngine.TestTools;
using NUnit.Framework;
using System.Collections;

/// <summary>
/// This class contains some basic player movement tests. Testing based on input will need to be done manually.
/// </summary>
public class PlayerMovementTest
{
    GameObject player;
    Player playerObj;
    Rigidbody2D rigidbody;
    EntityActionManager entityActionManager;
    EntityAction playerMove;

    /// <summary>
    /// Initialize the Player GameObject and its EntityActionManager for use in all the tests.
    /// </summary>
    [SetUp]
    public void Init()
    {
        SettingsManager.LoadSettings(""); //will load default settings since loading from file "" will fail
        player = new GameObject("Player", typeof(BoxCollider2D));
        entityActionManager = player.AddComponent<EntityActionManager>();
        playerMove = Resources.Load<EntityAction>("PlayerMove");
        rigidbody = null;
        playerObj = Player.CreateInstance<Player>();
        playerObj.actions = new EntityAction[] { playerMove };
        entityActionManager.entity = playerObj;
        Assert.IsNotNull(playerMove);
        Assert.IsNotNull(player.GetComponent<BoxCollider2D>());
    }

    /// <summary>
    /// Clean up everything after each test.
    /// </summary>
    [TearDown]
    public void Cleanup()
    {
        if (playerMove != null)
        {
            Resources.UnloadAsset(playerMove);
        }

        if (player != null)
        {
            GameObject.Destroy(player);
        }

        rigidbody = null;
    }

    /// <summary>
    /// Initialize the player's rigidbody with some defined constraints.
    /// </summary>
    void InitRigidbody()
    {
        rigidbody = player.AddComponent<Rigidbody2D>();
        rigidbody.bodyType = RigidbodyType2D.Dynamic;
        rigidbody.gravityScale = 0;
        rigidbody.angularDrag = 0;
        rigidbody.drag = 0;
        rigidbody.mass = 1;
        rigidbody.constraints = RigidbodyConstraints2D.FreezeRotation;
    }

    /// <summary>
    /// Tests whether the PlayerMoveAction is valid. Should always return true for now.
    /// </summary>
    [Test]
    public void TestValidate()
    {
        bool canMove = playerMove.Validate(player);
        Assert.AreEqual(true, canMove);
    }


    /// <summary>
    /// Tests whether the PlayerMoveAction fails when there is no rigidbody.
    /// </summary>
    [Test]
    public void TestFailMove()
    {
        bool executed = playerMove.Execute(player);
        Assert.AreEqual(false, executed);
    }

    /// <summary>
    /// Tests whether the PlayerMoveAction succeeds when there is a rigidbody.
    /// </summary>
    [Test]
    public void TestSuccessMove()
    {
        if (rigidbody == null)
        {
            InitRigidbody();
        }
        bool executed = playerMove.Execute(player);
        Assert.AreEqual(true, executed);
    }

    /// <summary>
    /// Tests whether the player doesn't move after a frame when there is no input.
    /// </summary>
    /// <returns></returns>
    [UnityTest]
    public IEnumerator TestNoInput()
    {
        if (rigidbody == null)
        {
            InitRigidbody();
        }
        Vector2 originalPosition = rigidbody.position;
        yield return new WaitForFixedUpdate();
        Assert.AreEqual(originalPosition, rigidbody.position);
        Assert.IsTrue(rigidbody.velocity.magnitude <= float.Epsilon);
    }
}
