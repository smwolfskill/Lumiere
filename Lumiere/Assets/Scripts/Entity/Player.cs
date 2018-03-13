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

    override public GameObject Spawn(Vector2 location, float maxHealth = 100.0f)
    {
        //Spawn the player GameObject, then set its tag
        GameObject playerGameObject = base.Spawn(location, maxHealth);
        playerGameObject.tag = "Player";

        //Tell the camera object that the player has been spawned
        Camera.main.GetComponent<CameraFollow>().FindPlayerTransform();
        PlayerObject entityObj = new PlayerObject(playerGameObject, maxHealth);   
        return playerGameObject;
    }
}
