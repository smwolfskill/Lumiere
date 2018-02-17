using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EntityActionManager : MonoBehaviour {

	//public Entity entity;
	public Action action;

    // Update is called once per frame
    void Update ()
    {
        Action[] actions = new Action[] { action };
        ExecuteValidActions(actions);
	}

    public bool ExecuteValidActions(Action[] actions)
    {
        if (this.gameObject == null)
        {
            return false;
        }

        foreach (Action action in actions) 
        {

            if (action.Validate(this.gameObject))
            {

                action.Execute(this.gameObject);
            }
        }

        return true;
    }
}
