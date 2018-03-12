using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class NPC : Entity 
{
    public State initialState;
    
    public override GameObject Spawn (Map map, Vector2 location)
    {
        GameObject npc = base.Spawn (map, location);
        StateController stateController = npc.AddComponent<StateController> ();
        stateController.currentState = this.initialState;
        stateController.map = map;
        return npc;
    }
}
