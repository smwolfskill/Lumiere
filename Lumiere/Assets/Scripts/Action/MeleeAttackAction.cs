using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MeleeAttackAction : EntityAction 
{
    /// <summary>
    /// The base attack damage that this entity will deal.
    /// </summary>
    public float attackDamage;

    /// <summary>
    /// The attack speed of this entity, described as number of attacks per second.
    /// </summary>
    public float attackSpeed;

    private float lastAttackTime = 0f;

    /// <summary>
    /// Checks whether this entity can attack or not.
    /// </summary>
    /// <param name="obj">The GameObject that wants to execute this action.</param>
    /// <returns>Returns true if the entity can attack, based on attackSpeed and lastAttackTime and false otherwise.</returns>
    public override bool Validate (GameObject obj)
    {
        throw new System.NotImplementedException ();
    } 

    /// <summary>
    /// Performs a melee attack.
    /// </summary>
    /// <param name="obj">The GameObject that wants to execute this action.</param>
    /// <returns>Returns true if this action is executed successfully, false otherwise.</returns>
    public override bool Execute (GameObject obj)
    {
        throw new System.NotImplementedException ();
    }
}
