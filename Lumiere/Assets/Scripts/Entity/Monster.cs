using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu (menuName = "Lumiere/Entity/NPC/Monster")]
public class Monster : NPC
{
    public GameObject damageParticles;

    override public GameObject Spawn(Map map, Vector2 location)
    {
        GameObject monster = base.Spawn(map, location);
        GameObject particles = Instantiate(damageParticles, monster.transform);
        monster.layer = LayerMask.NameToLayer("Enemy");
        MonsterObject obj = new MonsterObject(monster, maxHealth);
        
        monster.GetComponent<EntityHealthManager>().entityObj = obj;
        return monster;
    }
  
}
