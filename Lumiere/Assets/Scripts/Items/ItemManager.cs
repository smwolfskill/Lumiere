using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ItemManager : MonoBehaviour 
{
    public static float PICKUP_RADIUS = 0.2f; //radius outward from the gameobject within which the item can be picked up

    public Sprite groundSprite;
    public GameItem item;
    //public bool onGround;
    private SpriteRenderer renderer;
    private BoxCollider2D collider;

	// Use this for initialization
	void Start () {
        //item = new GameItem(groundSprite, groundSprite, "Test item", "This is a test item", 1.0, GameItem.ItemRarity.COMMON, 2, 5, 6);
        renderer = GetComponent<SpriteRenderer>();
        renderer.sprite = item.GroundSprite;
        collider = GetComponent<BoxCollider2D>();
        collider.edgeRadius = PICKUP_RADIUS;
	}
	
	// Update is called once per frame
	void Update () {
		//probably don't need
	}

    void OnTriggerEnter2D(Collider2D other)
    {
        Entity collidingEntity = getCollidingEntity(other);
        if(collidingEntity != null)
        {
            collidingEntity.nearbyItems.AddLast(gameObject);
        }
        Debug.Log(other.tag + "entered pickup area");
    }

    void OnTriggerExit2D(Collider2D other)
    {
        Entity collidingEntity = getCollidingEntity(other);
        if(collidingEntity != null)
        {
            collidingEntity.nearbyItems.Remove(gameObject);
        }
        Debug.Log("exit pickup area");
    }

    private Entity getCollidingEntity(Collider2D collider)
    {
        GameObject collidingObj = collider.gameObject;
        EntityActionManager entityActionManager = collidingObj.GetComponent<EntityActionManager>();
        if(entityActionManager != null)
        {
            return entityActionManager.entity;
        }
        return null;
    }

}
