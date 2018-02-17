using UnityEngine;
using UnityEngine.TestTools;
using NUnit.Framework;
using System.Collections;

public class PlayerMovementTest
{
    GameObject player;
    Rigidbody2D rigidbody;
    EntityActionManager entityActionManager;
    EntityAction playerMove;

    [SetUp]
	public void Init()
    {
        player = new GameObject("Player", typeof(BoxCollider2D));
        entityActionManager = player.AddComponent<EntityActionManager>();
        playerMove = Resources.Load<EntityAction>("PlayerMove");
        rigidbody = null;
        entityActionManager.entityAction = playerMove;
        Assert.IsNotNull(playerMove);
        Assert.IsNotNull(player.GetComponent<BoxCollider2D>());
    }

    [TearDown]
    public void Cleanup()
    {
        if (playerMove != null)
        {
            Resources.UnloadAsset(playerMove);
        }

        if(player != null)
        {
            GameObject.Destroy(player);
        }

        rigidbody = null;
    }

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

    [Test]
    public void TestValidate()
    {
        bool canMove = playerMove.Validate(player);
        Assert.AreEqual(true, canMove);
    }

    [Test]
    public void TestFailMove()
    {
        bool executed = playerMove.Execute(player);
        Assert.AreEqual(false, executed);
    }

    [Test]
    public void TestSuccessMove()
    {
        if (rigidbody == null) { InitRigidbody(); }
        bool executed = playerMove.Execute(player);
        Assert.AreEqual(true, executed);
    }

    [UnityTest]
    public IEnumerator TestNoInput()
    {
        if (rigidbody == null) { InitRigidbody(); }
        Vector2 originalPosition = rigidbody.position;
        yield return new WaitForFixedUpdate();
        Assert.AreEqual(originalPosition, rigidbody.position);
        Assert.IsTrue(rigidbody.velocity.magnitude <= float.Epsilon);
    }
}
