using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EntityActionManager : MonoBehaviour
{

    public Entity entity;

    // Update is called once per frame
    void Update ()
    {
        ExecuteValidActions(entity.actions);
    }

    /// <summary>
    /// Executes all valid EntityActions given an array of EntityAction
    /// </summary>
    /// <param name="entityActions">An array of EntityAction</param>
    /// <returns>Returns false if this function fails, otherwise always returns true, even if no actions are executed.</returns>
    public bool ExecuteValidActions(EntityAction[] entityActions)
    {
        if (this.gameObject == null)
        {
            return false;
        }

        foreach (EntityAction entityAction in entityActions) 
        {

            if (entityAction.Validate(this.gameObject))
            {

                entityAction.Execute(this.gameObject);
            }
        }

        return true;
    }
}
