using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ChaseAction : MonsterMoveAction 
{
    /// <summary>
    /// The distance to stop chasing the target.
    /// </summary>
    public float stoppingDistance;

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
        throw new System.NotImplementedException ();
    }
}
