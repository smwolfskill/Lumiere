using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.TestTools;
using NUnit.Framework;

/// <summary>
/// Basic testing class for entity spawning.
/// </summary>
public class PlayerAnimationTest 
{
    GameObject player;
    Rigidbody2D rigidbody;
    Animator anim;
    RuntimeAnimatorController playerAnimationController;

    [SetUp]
    public void Init()
    {
        player = new GameObject("player");
        player.AddComponent<SpriteRenderer>();
        rigidbody = player.AddComponent<Rigidbody2D>();
        rigidbody.gravityScale = 0f;
        rigidbody.angularDrag = 0f;
        rigidbody.freezeRotation = true;

        anim = player.AddComponent<Animator>();
        anim.runtimeAnimatorController = Resources.Load<RuntimeAnimatorController>("Animations/Player");
        playerAnimationController = anim.runtimeAnimatorController;
        //Debug.Log("RuntimeAnimController: " + anim.runtimeAnimatorController.ToString());
        MovementAnimation moveAnim = player.AddComponent<MovementAnimation> ();
    }

    [TearDown]
    public void Cleanup()
    {
        if (playerAnimationController != null)
            Resources.UnloadAsset(playerAnimationController);
        if (player != null)
            GameObject.Destroy(player);
    }

    /// <summary>
    /// Tests whether the player animation transitions properly up.
    /// </summary>
    [UnityTest]
    public IEnumerator TestMoveUp()
    {
        rigidbody.velocity = new Vector2(0.0f, 10.0f);
        yield return new WaitForSeconds(0.1f);
        Assert.IsTrue(anim.GetCurrentAnimatorStateInfo(0).IsName("PlayerWalkUp"));
        rigidbody.velocity = new Vector2(0.0f, 0.05f);
        yield return new WaitForSeconds(0.1f);
        Assert.IsTrue(anim.GetCurrentAnimatorStateInfo(0).IsName("PlayerIdleUp"));

    }

    /// <summary>
    /// Tests whether the player animation transitions properly down.
    /// </summary>
    [UnityTest]
    public IEnumerator TestMoveDown()
    {
        rigidbody.velocity = new Vector2(0.0f, -10.0f);
        yield return new WaitForSeconds(1.1f);
        Assert.IsTrue(anim.GetCurrentAnimatorStateInfo(0).IsName("PlayerWalkDown"));
        rigidbody.velocity = new Vector2(0.0f, -0.05f);
        yield return new WaitForSeconds(1.1f);
        Assert.IsTrue(anim.GetCurrentAnimatorStateInfo(0).IsName("PlayerIdleDown"));
    }

    /// <summary>
    /// Tests whether the player animation transitions properly left.
    /// </summary>
    [UnityTest]
    public IEnumerator TestMoveLeft()
    {
        rigidbody.velocity = new Vector2(-10.0f, 0.0f);
        yield return new WaitForSeconds(1.1f);
        Assert.IsTrue(anim.GetCurrentAnimatorStateInfo(0).IsName("PlayerWalkLeft"));
        rigidbody.velocity = new Vector2(-0.05f, 0.0f);
        yield return new WaitForSeconds(1.1f);
        Assert.IsTrue(anim.GetCurrentAnimatorStateInfo(0).IsName("PlayerIdleLeft"));
    }

    /// <summary>
    /// Tests whether the player animation transitions properly right.
    /// </summary>
    [UnityTest]
    public IEnumerator TestMoveRight()
    {
        rigidbody.velocity = new Vector2(10.0f, 0.0f);
        yield return new WaitForSeconds(1.1f);
        Assert.IsTrue(anim.GetCurrentAnimatorStateInfo(0).IsName("PlayerWalkRight"));
        rigidbody.velocity = new Vector2(0.05f, 0.0f);
        yield return new WaitForSeconds(1.1f);
        Assert.IsTrue(anim.GetCurrentAnimatorStateInfo(0).IsName("PlayerIdleRight"));
    }
}