using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ItemManager : MonoBehaviour
{
    public static float PICKUP_RADIUS = 0.5f; //radius outward from the gameobject within which the item can be picked up

    public Sprite groundSprite;
    public GameItem item;
    //public bool onGround;
    private SpriteRenderer renderer;
    private BoxCollider2D collider;

    // Use this for initialization
    void Start()
    {
        renderer = GetComponent<SpriteRenderer>();
        renderer.sprite = item.GroundSprite;
        collider = GetComponent<BoxCollider2D>();
        collider.edgeRadius = PICKUP_RADIUS;
    }

    void OnTriggerEnter2D(Collider2D other)
    {
        Entity collidingEntity = getCollidingEntity(other);
        if (collidingEntity != null)
        {
            collidingEntity.nearbyItems.AddLast(gameObject);
        }
        //Debug.Log(other.tag + "entered pickup area");
    }

    void OnTriggerExit2D(Collider2D other)
    {
        Entity collidingEntity = getCollidingEntity(other);
        if (collidingEntity != null)
        {
            collidingEntity.nearbyItems.Remove(gameObject);
        }
        //Debug.Log("exit pickup area");
    }

    private Entity getCollidingEntity(Collider2D collider)
    {
        GameObject collidingObj = collider.gameObject;
        EntityActionManager entityActionManager = collidingObj.GetComponent<EntityActionManager>();
        if (entityActionManager != null)
        {
            return entityActionManager.entity;
        }
        return null;
    }

}
