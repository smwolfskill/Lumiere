using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.TestTools;
using NUnit.Framework;


class AIBehaviorTest
{
    GameObject player;
    GameObject monster;
    ChaseDecision testChase;
    AttackDecision testAttack;
    State chaseState;
    State attackState;
    State idleState;
    StateController handler;

    /// <summary>
    /// Setup phase for constructing tests. Called before each test.
    /// </summary>
    [SetUp]
    public void Init()
    {
        player = new GameObject("Player", typeof(BoxCollider2D));
        monster = new GameObject("Monster", typeof(BoxCollider2D));
        player.tag = "Player";
        handler = monster.AddComponent<StateController>();

        testChase = Resources.Load<ChaseDecision>("Decisions/TestChaseDecision");
        testAttack = Resources.Load<AttackDecision>("Decisions/TestAttackDecision");

        chaseState = Resources.Load<State>("States/ChaseTest");
        attackState = Resources.Load<State>("States/AttackTest");
        idleState = Resources.Load<State>("States/IdleTest");

        Assert.IsNotNull(player);
        Assert.IsNotNull(monster);
        Assert.IsNotNull(handler);
        Assert.IsNotNull(testChase);
        Assert.IsNotNull(testAttack);
        Assert.IsNotNull(chaseState);
        Assert.IsNotNull(attackState);
        Assert.IsNotNull(idleState);
    }

    /// <summary>
    /// Cleanup steps for testing.
    /// </summary>
    [TearDown]
    public void Cleanup()
    {
        if (testChase != null)
            Resources.UnloadAsset(testChase);

        if (testAttack != null)
            Resources.UnloadAsset(testAttack);

        if (chaseState != null)
            Resources.UnloadAsset(chaseState);

        if (attackState != null)
            Resources.UnloadAsset(attackState);

        if (idleState != null)
            Resources.UnloadAsset(idleState);

        if (monster != null)
            GameObject.Destroy(monster);

        if (player != null)
            GameObject.Destroy(monster);
    }

    /// <summary>
    /// Tests if chase decisions are calculated correctly.
    /// </summary>
    [Test]
    public void TestChaseDecision()
    {
        // 1. False, player is not close enough to chase.
        player.transform.position.Set(5, 10, 0);
        monster.transform.position.Set(5, 20, 0);
        Assert.IsFalse(testChase.Decide(handler));

        // 2. True, player is close enough to chase.
        player.transform.position.Set(5, 10, 0);
        monster.transform.position.Set(5, 14, 0);
        Assert.IsTrue(testChase.Decide(handler));
    }

    /// <summary>
    /// Tests if attack decisions are calculated correctly.
    /// </summary>
    [Test]
    public void TestAttackDecision()
    {
        // 1. False, player is not close enough to attack.
        player.transform.position.Set(5, 10, 0);
        monster.transform.position.Set(5, 14, 0);
        Assert.IsFalse(testAttack.Decide(handler));

        // 2. True, player is close enough to attack.
        player.transform.position.Set(5, 10, 0);
        monster.transform.position.Set(5, 11, 0);
        Assert.IsTrue(testAttack.Decide(handler));
    }

    /// <summary>
    /// Tests if state transitions occur correctly.
    /// </summary>
    [Test]
    public void TestStateControlFlow()
    {
        // 1. Starts Idle, does not transition. (Chase Decision Fails)
        player.transform.position.Set(5, 10, 0);
        monster.transform.position.Set(5, 20, 0);

        handler.currentState = idleState;
        handler.currentState.UpdateState(handler);

        Assert.AreSame(idleState, handler.currentState);

        // 2. Chase Decision succeeds, is in Chase state.
        monster.transform.position.Set(5, 14, 0);

        handler.currentState.UpdateState(handler);

        Assert.AreSame(chaseState, handler.currentState);

        // 3. Chase Decision succeeds, Attack Decision fails, is in Chase state.
        handler.currentState.UpdateState(handler);
        Assert.AreSame(chaseState, handler.currentState);

        // 4. Chase Decision Fails, returns to Idle state.
        monster.transform.position.Set(5, 16, 0);

        handler.currentState.UpdateState(handler);

        Assert.AreSame(idleState, handler.currentState);

        // 5. Is returned to chase state. Attack decision succeeds. Is now in attack state.
        monster.transform.position.Set(5, 11, 0);
        handler.currentState.UpdateState(handler);
        handler.currentState.UpdateState(handler);

        Assert.AreSame(attackState, handler.currentState);
    }
}
