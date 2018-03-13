using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class NPC : Entity 
{
    public State initialState;
    
    override public GameObject Spawn (Map map, Vector2 location, float maxHealth)
    {
        GameObject npc = base.Spawn (map, location, maxHealth);
        StateController stateController = npc.AddComponent<StateController> ();
        stateController.currentState = this.initialState;
        stateController.map = map;
        return npc;
    }
}
