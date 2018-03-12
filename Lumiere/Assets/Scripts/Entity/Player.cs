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
    public override GameObject Spawn (Map map, Vector2 location)
    {
        GameObject player = base.Spawn (map, location);
        EntityActionManager actionManager = player.AddComponent<EntityActionManager> ();
        actionManager.entity = this;
        return player;
    }
}
