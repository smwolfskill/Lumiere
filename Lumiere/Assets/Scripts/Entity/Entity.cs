using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;

public abstract class Entity : BaseObject 
{
    public EntityAction[] actions;     //will hold all actions that this entity can perform
    public Vector2 colliderSize; // the size of the entity's collider
    public Inventory inventory;
    public LinkedList<GameObject> nearbyItems = new LinkedList<GameObject>(); //list of items that this entity could pickup if desired. Will be used by AI mainly
    public EntityObject entityObject;
    public EntityDropGen entityDropGen;

    /// <summary>
    /// Spawn the entity at the specified location.
    /// </summary>
    /// <param name="location">Location to spawn this entity.</param>
    /// <returns>Returns the GameObject representing this entity.</returns>
    virtual public GameObject Spawn(Map map, Vector2 location)
    {

        GameObject entity = new GameObject (this.name);
        entity.transform.position = location;

        SpriteRenderer renderer = entity.AddComponent<SpriteRenderer> ();
        renderer.sortingLayerName = "Entities";
        renderer.sortingOrder = 0;//1; //1 prevents player from picking up items that they are directly on top of

        EntitySpriteManager entitySpriteManager = entity.AddComponent<EntitySpriteManager> ();
        entitySpriteManager.entity = this;

        BoxCollider2D collider = entity.AddComponent<BoxCollider2D> ();
        collider.size = colliderSize;

        Rigidbody2D rigidbody = entity.AddComponent<Rigidbody2D> ();
        rigidbody.gravityScale = 0f;
        rigidbody.angularDrag = 0f;
        rigidbody.freezeRotation = true;

        entityDropGen.SetLevelAndDifficulty(map); //Update entity's loot gen parameters with this map's level & difficulty.

        return entity;
    }


}
