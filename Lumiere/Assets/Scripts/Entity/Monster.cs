using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu (menuName = "Lumiere/Entity/NPC/Monster")]
public class Monster : NPC
{

    override public GameObject Spawn(Map map, Vector2 location, float maxHealth = 10.0f)
    {
        //TODO: add EntityObjectManager, instantiate a entityObject, attach the entityObject
        //      to the EntityObjectManager, somewhere else keep track of the entityObject.

        return base.Spawn(map, location, maxHealth);
    }
  
}
