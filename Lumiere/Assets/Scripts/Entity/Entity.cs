using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class Entity : BaseObject 
{
    public EntityAction[] actions;     //will hold all actions that this entity can perform
    public Vector2 colliderSize; // the size of the entity's collider
    public AudioClip deathSound;
    public Inventory inventory;
    public LinkedList<GameObject> nearbyItems = new LinkedList<GameObject>(); //list of items that this entity could pickup if desired. Will be used by AI mainly
    public EntityObject entityObject;
    public EntityDropGen entityDropGen;

    /// <summary>
    /// Holds fields governing type, quality and quantity of items dropped upon entity death.
    /// </summary>
    [System.Serializable]
    public class EntityDropGen
    {
        public int quality = 1;
        public int minItems;
        public int maxItems;
        public GameItem.ItemRarity minRarity;
        public double healthPotionChance; //percentage chance of one item dropped being a health potion, in range [0, 1] inclusive.

        public EntityDropGen(int quality, int minItems, int maxItems, GameItem.ItemRarity minRarity, double healthPotionChance)
        {
            this.quality = quality;
            this.minItems = minItems;
            this.maxItems = maxItems;
            this.minRarity = minRarity;
            this.healthPotionChance = healthPotionChance;
        }

        public GameItem[] GenerateLoot()
        {
            return ItemSpawner.GenerateLootBag(-1, quality, minItems, maxItems, minRarity, false, healthPotionChance);
        }
    }


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

        AudioSource aSource = entity.AddComponent<AudioSource> ();
        aSource.playOnAwake = false;
        aSource.clip = deathSound;

        Rigidbody2D rigidbody = entity.AddComponent<Rigidbody2D> ();
        rigidbody.gravityScale = 0f;
        rigidbody.angularDrag = 0f;
        rigidbody.freezeRotation = true;


        //EntityObject entityObj = new EntityObject(entity, maxHealth);
        // TODO: add to map

        // EntityObject eo = new EntityObject(entity);
        // after this function terminates, we lose the pointer to eo!
        // so, we need to hold onto it; our solution: put it into map
        //        to do that, add an array in map: EntityObject[] entityObjects;
        //        Map::addEntityObject()
        //        Map::removeEntityObject()

        return entity;
    }


}
