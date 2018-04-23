using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Actions/PlayerAttack")]
public class PlayerAttack : EntityAction
{
    private WeaponItem weapon;
    private EntityHealthManager targetHM;
	private Transform targetTransform;
	private float animationSpeed = 2;

    /// <summary>
    /// Checks whether the player can attack a monster
    /// </summary>
    /// <param name="_player">The Player's GameObject</param>
    /// <returns>true if the clicked on monster is within the hotbar's selected item's attack range</returns>
    public override bool Validate(GameObject _player)
    {
        Camera main = Camera.main;
        bool attack = Input.GetButtonDown("Fire1");
        Vector3 mousePosition = Input.mousePosition;
        EntitySpriteManager em;
        Player player;
        EquipmentManager equips;

        em = _player.GetComponent<EntitySpriteManager>();
        player = (Player)em.entity;
        equips = (player.entityObject as PlayerObject).EquipmentManager;

        if (attack)
        {
            Vector2 origin = _player.transform.position;
            Vector2 worldPointClicked = main.ScreenToWorldPoint(mousePosition);
            Vector2 direction = (worldPointClicked - origin);
            direction.Normalize();
            float attackRange = 1f; // melee range (no weapon)
            UsableItem selected = equips.GetSelectedItem();
            WeaponItem weapon = selected is WeaponItem ?
                (WeaponItem)selected : null;

            // check if hotbar is a weapon and update the attack range
            if (weapon != null && weapon.SetYet())
            {
                attackRange = Mathf.Max((float) weapon.AttackRange, attackRange);
            }

            RaycastHit2D hit2D = Physics2D.Raycast(origin, direction, attackRange, 1 << LayerMask.NameToLayer("Enemy"));
            Debug.DrawLine(origin, worldPointClicked, Color.green, 0.5f);
            if (hit2D.collider != null)
            {
                GameObject target = hit2D.collider.gameObject;
                em = target.GetComponent<EntitySpriteManager>();
                Entity ent = em != null ? em.entity : null;
                if (ent != null && ent is Monster)
                {
                    this.weapon = weapon;
                    this.target = ent;
                    this.targetHM = target.GetComponent<EntityHealthManager>();
                    this.targetTransform = target.transform;
                    return true;

                }
            }
        }
        return false;
    }

    /// <summary>
    /// Attack the target
    /// </summary>
    /// <param name="_player">The Player's GameObject</param>
    /// <returns>true if the attack succeeded (e.g. we didn't run out of arrows)</returns>
    public override bool Execute(GameObject _player)
    {
        Player player;
        float dmg;

        player = (Player)_player.GetComponent<EntitySpriteManager>().entity;
        dmg = weapon == null ? player.FisticuffDamage : weapon.Damage;

		GameObject attackAnimationObj = player.AttackAnimationObject;

		//attackAnim.CreateAnimation(player, attackAnimationObj, weapon, targetTransform, animationSpeed);
		attackAnimationObj.GetComponent<AttackAnimation>().CreateAnimation(player, attackAnimationObj, this.weapon, this.targetTransform, this.animationSpeed);
		attackAnimationObj.GetComponent<Animation>().Play("attack");

        targetHM.InflictDamage(dmg);

		//Debug.Log(tmpSprite.GetInstanceID());
        
        return true;
    }


}
