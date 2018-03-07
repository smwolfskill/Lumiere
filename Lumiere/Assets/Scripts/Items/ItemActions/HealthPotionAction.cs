using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class HealthPotionAction : ItemAction
{
    /// <summary>
    /// Checks whether this action should be executed or not for the specified GameObject.
    /// </summary>
    /// <param name="obj">The GameObject that wants to execute this action.</param>
    /// <returns>Returns true if this action should be executed, false otherwise.</returns>
    public override bool Validate(GameObject obj)
    {
        return true;
    }

    /// <summary>
    /// Executes this action for the specified GameObject.
    /// </summary>
    /// <param name="obj">The GameObject that wants to execute this action.</param>
    /// <returns>Returns true if this action is executed successfully, false otherwise.</returns>
    public override bool Execute(GameObject obj)
    {
        //TODO when there are Entity health bars...
        return true;
    }
}
