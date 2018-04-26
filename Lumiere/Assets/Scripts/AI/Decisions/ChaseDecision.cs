using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/AI/Decisions/Chase Decision")]
public class ChaseDecision : Decision
{
    /// <summary>
    /// The chase distance that can be modified in the editor or specified as a parameter of the entity to make this decision.
    /// </summary>
    public float chaseDistance = 5f;

    public float attackRange = 1f;

    /// <summary>
    /// The entity scans for the target. If the target is within the specified distance, return true. If the target is not found or not within the specified distance, return false.
    /// </summary>
    /// <param name="stateController">The state controller of the deciding entity.</param>
    public override bool Decide(StateController stateController)
    {
        //TODO: implement
        GameObject entity = stateController.gameObject;
        if (entity == null)
        {
            return false;
        }
        Vector2 position = entity.transform.position;
        GameObject player = GameObject.FindGameObjectWithTag("Player");
        if (player == null)
        {
            return false;
        }

        Vector2 playerPosition = player.transform.position;
        // change in unity
        float distance = Vector2.Distance(position, playerPosition);

        return distance <= chaseDistance && distance > attackRange;
    }

}
