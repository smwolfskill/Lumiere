using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PlayerObject : EntityObject
{
    private GameObject healthBar;

    public PlayerObject(GameObject existingGameObject, float maxHealth) : base(existingGameObject, maxHealth)
    {
        this.inventory = new Inventory(5,3);
        this.healthBar = GameObject.Find("PanelHealthBarFill");
        Camera.main.GetComponent<CameraFollow>().SetTargetTransform(existingGameObject.transform);
        GameObject panel = GameObject.FindGameObjectWithTag("InventoryPanel");
        panel.GetComponent<InventoryPanel>().SetInitialInventory(this.inventory);
    }

    public override void InflictDamage(float damageAmount)
    {
        base.InflictDamage(damageAmount);
        //Debug.Log ("Inflicting damage! Health: " + currHealth + " Max Health: " + maxHealth);
        UpdateHealthBar();
    }

    public override void Heal(float healAmount)
    {
        base.Heal(healAmount);
        UpdateHealthBar();
    }

    private void UpdateHealthBar()
    {
        this.healthBar.GetComponent<HealthBarManager>().SetHealth(currHealth / maxHealth);
    }
}
