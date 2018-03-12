using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AttackDecision : Decision 
{
    /// <summary>
    /// The attack range that can be modified in the editor or added as a parameter of the entity to make this decision.
    /// For ranged entities, this will be a larger number.
    /// </summary>
    public float attackRange = 1f;

    /// <summary>
    /// If this entity is within attack range of the target and can "see" the target, return true. If the target does not exist or it is not within the attack range, then return false.
    /// </summary>
    /// <param name="stateController">The state controller for the deciding entity.</param>
    public override bool Decide (StateController stateController)
    {
        //TODO: implement
        //TODO: implement
        GameObject entity = stateController.gameObject;
        if(entity == null)
        {
            return false;
        }
        Vector2 position = entity.transform.position;
        GameObject player = GameObject.FindGameObjectWithTag("Player");
        if(player == null)
        {
            return false;
        }

        Vector2 playerPosition = player.transform.position;
        // change in unity
        float distance = Vector2.Distance(position, playerPosition);

        return distance <= attackRange;
    }
}
