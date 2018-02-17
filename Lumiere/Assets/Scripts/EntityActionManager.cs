using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EntityActionManager : MonoBehaviour {

	//public Entity entity;
	public EntityAction entityAction;

    // Update is called once per frame
    void Update ()
    {
        EntityAction[] actions = new EntityAction[] { entityAction };
        ExecuteValidActions(actions);
	}

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
