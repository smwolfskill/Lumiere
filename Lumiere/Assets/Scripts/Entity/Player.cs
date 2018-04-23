using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu (menuName = "Lumiere/Entity/Player")]
public class Player : Entity 
{
    [SerializeField]
    private float _FisticuffDamage;
    [SerializeField]
    private float maxHealth;

    public float FisticuffDamage
    {
        get
        {
            return _FisticuffDamage;
        }
        private set
        {
            _FisticuffDamage = value;
        }
    }

    override public GameObject Spawn(Map map, Vector2 location)
    {
        GameObject player = base.Spawn (map, location);
        player.tag = "Player";
        EntityActionManager actionManager = player.AddComponent<EntityActionManager> ();
        actionManager.entity = this;
        PlayerObject entityObj = new PlayerObject(player, maxHealth);
        this.entityObject = entityObj;
        entityObj.entityDropGen = entityDropGen;
        EntityHealthManager healthManagerTest = player.GetComponent<EntityHealthManager> ();
        if (healthManagerTest == null)
        {
            EntityHealthManager healthManager = player.AddComponent<EntityHealthManager> ();
            healthManager.entityObj = entityObj;
        }
        else
        {
            healthManagerTest.entityObj = this.entityObject;
        }
        player.AddComponent<EntityObjectManager>().entityObject = entityObj;

        return player;
    }
}
