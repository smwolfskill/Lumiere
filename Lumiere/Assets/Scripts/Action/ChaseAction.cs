using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ChaseAction : MonsterMoveAction 
{
    /// <summary>
    /// The distance to stop chasing the target.
    /// </summary>
    public float stoppingDistance;
    public float pathfindingThreshold = 3f;

    private Vector2 ourPosition;
    private Vector2 targetPosition;
    private Vector2 oldTargetPosition = new Vector2(int.MaxValue, int.MaxValue);

    /// <summary>
    /// Checks whether this action should be executed or not for the specified GameObject.
    /// </summary>
    /// <param name="obj">The GameObject that wants to execute this action.</param>
    /// <returns>Returns true if this action should be executed, false otherwise.</returns>
    public override bool Validate (GameObject obj)
    {
        throw new System.NotImplementedException ();
    }

    /// <summary>
    /// Executes this action for the specified GameObject.
    /// </summary>
    /// <param name="obj">The GameObject that wants to execute this action.</param>
    /// <returns>Returns true if this action is executed successfully, false otherwise.</returns>
    public override bool Execute (GameObject obj)
    {
        ourPosition = obj.transform.position;
        GameObject target = GameObject.FindGameObjectWithTag("Player");
        targetPosition = target.transform.position;
        float oldTargetDistance = Vector2.Distance(targetPosition, oldTargetPosition);
        if(oldTargetDistance > pathfindingThreshold)
        {
            //Do pathfinding
            return true;
        }

        return true;

    }
}
