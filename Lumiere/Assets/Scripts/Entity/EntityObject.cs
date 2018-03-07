using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class EntityObject
{

    public GameObject gameObject;
    protected float maxHealth;
    protected float currHealth;


    public EntityObject(float maxHealth)
    {
        //TODO
    }

    public EntityObject(GameObject existingGameObject, float maxHealth)
    {
        this.gameObject = existingGameObject;
        this.maxHealth = maxHealth;
        this.currHealth = maxHealth;
    }

    virtual public void InflictDamage(float damageAmount)
    {
        this.currHealth -= damageAmount;
        if (currHealth <= 0)
            this.Die();
    }

    virtual protected void Die()
    {

    }
}
