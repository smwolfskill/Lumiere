using System.Collections;
using System.Collections.Generic;
using UnityEngine;

<<<<<<< Lumiere/Assets/Scripts/Action.cs
public abstract class Action : ScriptableObject 
{
    /// <summary>
    /// Checks whether this action should be executed or not for the specified GameObject.
    /// </summary>
    /// <param name="obj">The GameObject that wants to execute this action.</param>
    /// <returns>Returns true if this action should be executed, false otherwise.</returns>
    public abstract bool Validate(GameObject obj);
    
    /// <summary>
    /// Executes this action for the specified GameObject.
    /// </summary>
    /// <param name="obj">The GameObject that wants to execute this action.</param>
    /// <returns>Returns true if this action is executed successfully, false otherwise.</returns>
    public abstract bool Execute(GameObject obj);

}
