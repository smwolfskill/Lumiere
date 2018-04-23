using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class EntityObject
{
    public GameObject gameObject;
    public Entity.EntityDropGen entityDropGen;
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

    public EntityObject(GameObject existingGameObject, float maxHealth)//, EntityDropGen entityDropGen)
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
        if (currHealth <= 0 && !isDead)
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
        //Drop random loot items if any
        DropLootAroundGameObject();
        //Destroy the GameObject
        Object.Destroy(gameObject);
        this.isDead = true;
    }

    virtual public bool IsDead()
    {
        return this.isDead;
    }

    /// <summary>
    /// Drop random loot items around the entity's GameObject, if any.
    /// </summary>
    virtual public void DropLootAroundGameObject()
    {
        if(entityDropGen != null && entityDropGen.maxItems > 0)
        {
            DropItemsAroundGameObject(entityDropGen.GenerateLoot());
        }
    }

    /// <summary>
    /// Drops the items around the entity's GameObject.
    /// </summary>
    /// <param name="itemsToDrop">Items to drop.</param>
    /// <param name="spacing">Spacing between items.</param>
    virtual public void DropItemsAroundGameObject(GameItem[] itemsToDrop)//, float spacing = 0.5f)
    {
        //For now, just drop items all on same location b/c don't want any items getting stuck in walls if entity is against one
        foreach(GameItem itemToDrop in itemsToDrop)
        {
            itemToDrop.CreateGameObject(gameObject.transform.position);
        }
    }
}
