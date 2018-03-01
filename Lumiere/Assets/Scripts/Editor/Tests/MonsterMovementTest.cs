using UnityEngine;
using UnityEngine.TestTools;
using NUnit.Framework;
using System.Collections;

/// <summary>
/// This class contains some basic Monster movement tests.
/// </summary>
public class MonsterMovementTest
{
    GameObject monster;
    Rigidbody2D rigidbody;
    EntityActionManager entityActionManager;
    Monster monsterObject;
    EntityAction randomMove;

    /// <summary>
    /// Initialize the Monster GameObject and its EntityActionManager for use in all the tests.
    /// </summary>
    [SetUp]
    public void Init()
    {
        monster = new GameObject("Monster", typeof(BoxCollider2D));
        entityActionManager = monster.AddComponent<EntityActionManager>();
        randomMove = Resources.Load<EntityAction>("RandomMove");
        rigidbody = null;

        monsterObject = Monster.CreateInstance<Monster> ();
        monsterObject.actions = new EntityAction[] { randomMove };
        entityActionManager.entity = monsterObject;

        monster.SetActive(false);
        Assert.IsNotNull(randomMove);
        Assert.IsNotNull(monster.GetComponent<BoxCollider2D>());
    }

    /// <summary>
    /// Clean up everything after each test.
    /// </summary>
    [TearDown]
    public void Cleanup()
    {
        if (randomMove != null)
        {
            Resources.UnloadAsset(randomMove);
        }

        if (monster != null)
        {
            GameObject.DestroyImmediate(monster);
        }

        rigidbody = null;
    }

    /// <summary>
    /// Initialize the Monster's rigidbody with some defined constraints.
    /// </summary>
    void InitRigidbody()
    {
        rigidbody = monster.AddComponent<Rigidbody2D>();
        rigidbody.bodyType = RigidbodyType2D.Dynamic;
        rigidbody.gravityScale = 0;
        rigidbody.angularDrag = 0;
        rigidbody.drag = 0;
        rigidbody.mass = 1;
        rigidbody.constraints = RigidbodyConstraints2D.FreezeRotation;
    }

    /// <summary>
    /// Tests whether the RandomMoveAction fails when there is no rigidbody.
    /// </summary>
    [Test]
    public void TestFailMove()
    {
        monster.SetActive(true);
        bool executed = randomMove.Execute(monster);
        Assert.AreEqual(false, executed);
    }

    /// <summary>
    /// Tests whether the RandomMoveAction succeeds when there is a rigidbody.
    /// </summary>
    [Test]
    public void TestSuccessMove()
    {
        monster.SetActive(true);
        if (rigidbody == null) { InitRigidbody(); }
        bool executed = randomMove.Execute(monster);
        Assert.AreEqual(true, executed);
    }
}
