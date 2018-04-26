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
    [SerializeField]
    private RuntimeAnimatorController animatorController;

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

	public GameObject AttackAnimationObject
	{
		get;
		private set;
	}

    override public GameObject Spawn(Map map, Vector2 location)
    {
        GameObject player = base.Spawn (map, location);
        player.tag = "Player";
        player.layer = LayerMask.NameToLayer ("Player");

        Animator anim = player.AddComponent<Animator> ();
        anim.runtimeAnimatorController = animatorController;
        MovementAnimation moveAnim = player.AddComponent<MovementAnimation> ();

		AttackAnimationObject = CreateAttackAnimGameObject();
		AttackAnimationObject.transform.SetParent(player.transform);
		AttackAnimationObject.transform.localPosition = Vector3.zero;

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

	private GameObject CreateAttackAnimGameObject()
	{
		GameObject attackAnimObj = new GameObject("PlayerAttackAnim", typeof(SpriteRenderer), typeof(Animation), typeof(AttackAnimation));
		//attackAnimObj.GetComponent<SpriteRenderer>().enabled = false;
		return attackAnimObj;
	}
}
