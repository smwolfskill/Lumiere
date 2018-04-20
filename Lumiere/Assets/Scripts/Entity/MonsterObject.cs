﻿using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MonsterObject : NPCObject
{
    private EnemyHealthBarManager healthManager;
    private Animator anim;

	public MonsterObject(GameObject existingGameObject, float maxHealth) : base(existingGameObject, maxHealth)
	{
        healthManager = existingGameObject.GetComponentInChildren<EnemyHealthBarManager>();
        anim = this.gameObject.GetComponent<Animator>();
	}

    public override void InflictDamage(float damageAmount)
    {
        base.InflictDamage(damageAmount);
        healthManager.SetHealth(currHealth / maxHealth);
    }

    public override void Heal(float healAmount)
    {
        base.Heal(healAmount);
        healthManager.SetHealth(currHealth / maxHealth);
    }

    protected override void Die()
    {
        this.gameObject.GetComponent<MovementAnimation>().enabled = false;
        anim.SetTrigger("TDie");
        this.isDead = true;
    }
}
