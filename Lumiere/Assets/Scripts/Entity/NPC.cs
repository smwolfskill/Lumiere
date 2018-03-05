using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class NPC : Entity 
{
    //TODO in future iteration: add AI selection code

    override public GameObject Spawn(Vector2 location, float maxHealth)
    {
        return base.Spawn(location, maxHealth);
    }

}
