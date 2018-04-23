using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu (menuName = "Lumiere/Entity/NPC/Monster")]
public class Monster : NPC
{
    override public GameObject Spawn(Map map, Vector2 location)
    {
        GameObject monster = base.Spawn(map, location);
        MonsterObject obj = new MonsterObject(monster, maxHealth);
        this.entityObject = obj;
        obj.entityDropGen = entityDropGen;
        monster.GetComponent<EntityHealthManager>().entityObj = obj;
        return monster;
    }
  
}
