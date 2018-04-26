using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu (menuName = "Lumiere/Actions/EntityActions/MeleeAttackAction")]
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

    private Timer timer;


    /// <summary>
    /// Checks whether this entity can attack or not.
    /// </summary>
    /// <param name="obj">The GameObject that wants to execute this action.</param>
    /// <returns>Returns true if the entity can attack, based on attackSpeed and lastAttackTime and false otherwise.</returns>
    public override bool Validate (GameObject obj)
    {
        timer = obj.GetComponent<Timer> ();
        if (timer == null) 
        {
            return false;    
        }

        if (!timer.Enabled) 
        {
            return true;
            timer.Reset ();
        }

        if (timer.HasExceeded(1.0f/attackSpeed)) 
        {
            return true;
        }

        return false;

    } 

    /// <summary>
    /// Performs a melee attack.
    /// </summary>
    /// <param name="obj">The GameObject that wants to execute this action.</param>
    /// <returns>Returns true if this action is executed successfully, false otherwise.</returns>
    public override bool Execute (GameObject obj)
    {
        GameObject player = GameObject.FindGameObjectWithTag ("Player");
        if (player == null) 
        {
            return false;
        }

        EntityHealthManager healthManager = player.GetComponent<EntityHealthManager> ();
        if (healthManager == null) 
        {
            return false;
        }

        healthManager.InflictDamage (attackDamage);
        timer.Reset ();
        return true;
    }
}
