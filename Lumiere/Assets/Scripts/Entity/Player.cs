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

    private GameObject panelHealthBarFill;

    public Player() : base()
    {
        panelHealthBarFill = GameObject.Find("PanelHealthBarFill");
    }

    override public GameObject Spawn(Vector2 location, float maxHealth = 100.0f)
    {
        return base.Spawn(location, maxHealth);
    }

    public override void InflictDamage(float damageAmount)
    {
        base.InflictDamage(damageAmount);
        UpdateHealthBar();
    }

    private void UpdateHealthBar()
    {
        panelHealthBarFill.GetComponent<HealthBarManager>().SetHealth(currHealth / maxHealth);

    }

}
