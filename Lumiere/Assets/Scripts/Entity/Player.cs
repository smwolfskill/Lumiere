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
    public override GameObject Spawn (Map map, Vector2 location, float maxHealth = 100.0f)
    {
        GameObject player = base.Spawn (map, location, maxHealth);
        player.tag = "Player";
        Camera.main.GetComponent<CameraFollow>().FindPlayerTransform();
        EntityActionManager actionManager = player.AddComponent<EntityActionManager> ();
        InventoryBehavior inventoryBehavior = player.AddComponent<InventoryBehavior> ();
        actionManager.entity = this;
        PlayerObject entityObj = new PlayerObject(player, maxHealth);  
        EntityHealthManager healthManager = player.AddComponent<EntityHealthManager> ();
        healthManager.entityObj = entityObj;
        return player;
    }
}
