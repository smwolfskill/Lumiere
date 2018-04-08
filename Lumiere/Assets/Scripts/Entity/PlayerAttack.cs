using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Actions/PlayerAttack")]
public class PlayerAttack : EntityAction
{
    private UsableItem weapon;

    /// <summary>
    /// Checks whether the player can attack a monster
    /// </summary>
    /// <param name="obj">The Player's GameObject</param>
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
                    float dist = Physics2D.Distance(objCollider,
                                                    hit2D.collider).distance;
                    float attackRange = 0.0f; // melee range (no weapon)
                    UsableItem selected = equips.GetSelectedItem();

                    // TODO: check if hotbar item is weapon
                    if (selected != null && selected.SetYet())
                    {
                        // TODO: update attack range
                    }
 
                    if (dist <= attackRange)
                    {
                        // we are within the attack range
                        this.weapon = selected;
                        this.target = ent;
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
    /// <param name="obj">The Player's GameObject</param>
    /// <returns>true if the attack succeeded (e.g. we didn't run out of arrows)</returns>
    public override bool Execute(GameObject obj)
    {
        // TODO: use weapon on target
        Debug.Log("Attack!");
        return true;
    }
}
