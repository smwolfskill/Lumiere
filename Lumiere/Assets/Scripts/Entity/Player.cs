using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu (menuName = "Lumiere/Entity/Player")]
public class Player : Entity 
{

	/* TODO in future iteration: 
	 + add inventory
	 + add currently equipped items
	 */ 
	//public Inventory inventory;
    /*public Player()
    {
        Debug.Log("Default player ctor");
        Spawn(new Vector2(0, 0));
    }*/

    override public GameObject Spawn(Map map, Vector2 location, float maxHealth = 100.0f)
    {
        GameObject player = base.Spawn (map, location, maxHealth);
        player.tag = "Player";
        EntityActionManager actionManager = player.AddComponent<EntityActionManager> ();
        actionManager.entity = this;
        PlayerObject entityObj = new PlayerObject(player, maxHealth);
        this.entityObject = entityObj;
        EntityHealthManager healthManager = player.AddComponent<EntityHealthManager> ();
        healthManager.entityObj = entityObj;
        player.AddComponent<EntityObjectManager>().entityObject = entityObj;

        return player;
    }
}
