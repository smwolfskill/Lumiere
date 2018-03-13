using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/AI/Decisions/Idle Decision")]
public class IdleDecision : Decision 
{
    /// <summary>
    /// The chase distance that can be modified in the editor or specified as a parameter of the entity to make this decision.
    /// </summary>
    public float chaseDistance = 5f;

    /// <summary>
    /// The entity scans for the target. If the target is within the specified distance, return false. If the target is within the specified distance, return true.
    /// </summary>
    /// <param name="stateController">The state controller of the deciding entity.</param>
    public override bool Decide (StateController stateController)
    {
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

        float distance = Vector2.Distance(position, playerPosition);

        return distance > chaseDistance;
    }
}
