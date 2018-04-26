using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;

public class EntityActionManager : MonoBehaviour
{
    public Entity entity;
    private UIBehavior uiBehavior = null;

    void Start()
    {
        GameObject uiCanvas = GameObject.FindGameObjectWithTag("UICanvas");
        if(uiCanvas != null)
        {
            uiBehavior = uiCanvas.GetComponent<UIBehavior>();
            if(uiBehavior == null)
            {
                throw new Exception("Error: UICanvas has no UIBehavior component!");
            }
        }
    }

    // Update is called once per frame
    void Update()
    {
        //Execute valid actions every frame, unless the in-game menu is open (game paused).
        if(uiBehavior == null || !uiBehavior.menuVisible)
        {
            ExecuteValidActions(entity.actions);
        }
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
