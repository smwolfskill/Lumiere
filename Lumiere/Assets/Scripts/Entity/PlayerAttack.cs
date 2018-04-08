using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Actions/PlayerAttack")]
public class PlayerAttack : EntityAction
{
    private WeaponItem weapon;
    private EntityHealthManager targetHM;

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
        Inventory inv;
        EquipmentManager equips;

        em = _player.GetComponent<EntitySpriteManager>();
        player = (Player)em.entity;
        inv = player.inventory;
        equips = inv.getEquipment();

        if (attack)
        {
            // Gather 3D mouse position and raycasting information
            Ray ray = main.ScreenPointToRay(mousePosition);
            Vector3 origin = _player.transform.position;
            Vector3 worldPointClicked = main.ScreenToWorldPoint(mousePosition);

            // Convert to 2D and detect any raycast hits on 2D colliders
            Ray2D ray2D =
                new Ray2D(new Vector2(ray.origin.x, ray.origin.y),
                          new Vector2(ray.direction.x, ray.direction.y));
            RaycastHit2D hit2D = Physics2D.Raycast(ray2D.origin,
                                                   ray2D.direction);
            if (hit2D.collider != null)
            {
                GameObject target = hit2D.collider.gameObject;
                em = target.GetComponent<EntitySpriteManager>();
                Entity ent = em != null ? em.entity : null;
                if (ent != null && ent is Monster)
                {
                    BoxCollider2D objCollider =
                        _player.GetComponent<BoxCollider2D>();
                    double dist = Physics2D.Distance(objCollider,
                                                     hit2D.collider).distance;
                    double attackRange = 0.0f; // melee range (no weapon)
                    UsableItem selected = equips.GetSelectedItem();
                    WeaponItem weapon = selected is WeaponItem ?
                        (WeaponItem)selected : null;

                    // check if hotbar is a weapon and update the attack range
                    if (weapon != null && weapon.SetYet())
                    {
                        attackRange = weapon.AttackRange;
                    }
 
                    if (dist <= attackRange)
                    {
                        // we are within the attack range
                        this.weapon = weapon;
                        this.target = ent;
                        this.targetHM =
                            target.GetComponent<EntityHealthManager>();
                        return true;
                    }
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
        targetHM.InflictDamage(dmg);
        
        return true;
    }
}
