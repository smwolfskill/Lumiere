using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class EntityObject
{

    public GameObject gameObject;
    protected float maxHealth;
    protected float currHealth;
    protected bool isDead;
	protected Inventory inventory;

    /*
    public EntityObject(float maxHealth)
    {
        //TODO
    }
    */

    public EntityObject(GameObject existingGameObject, float maxHealth)
    {
        this.gameObject = existingGameObject;
        this.maxHealth = maxHealth;
        this.currHealth = maxHealth;
        this.isDead = false;
		this.inventory = null;

        this.gameObject.AddComponent<EntityHealthManager>();
    }

    virtual public void InflictDamage(float damageAmount)
    {
        this.currHealth -= damageAmount;
        if (currHealth <= 0)
            this.Die();
    }

    virtual public void Heal(float healAmount)
    {
        if (!this.isDead)
        {
            this.currHealth += healAmount;
            if (currHealth > maxHealth)
                this.currHealth = this.maxHealth;
        }
    }

    virtual public float GetCurrHealth()
    {
        return this.currHealth;
    }

    virtual public float GetMaxHealth()
    {
        return this.maxHealth;
    }

    virtual protected void Die()
    {
        Object.Destroy(gameObject);
        this.isDead = true;
    }

    virtual public bool IsDead()
    {
        return this.isDead;
    }
}
