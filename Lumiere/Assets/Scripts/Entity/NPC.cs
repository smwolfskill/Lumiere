using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class NPC : Entity
{
    [SerializeField]
    protected float maxHealth;

    public State initialState;

    override public GameObject Spawn(Map map, Vector2 location)
    {
        GameObject npc = base.Spawn(map, location);
        StateController stateController = npc.AddComponent<StateController>();
        stateController.currentState = this.initialState;
        stateController.map = map;
        NPCObject obj = new NPCObject(npc, maxHealth);
        obj.entityDropGen = entityDropGen;
        this.entityObject = obj;
        npc.GetComponent<EntityHealthManager>().entityObj = obj;
        return npc;
    }
}
