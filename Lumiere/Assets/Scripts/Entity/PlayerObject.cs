using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PlayerObject : EntityObject
{
    private GameObject healthBar;
    protected EquipmentManager equipmentManager;

    public PlayerObject(GameObject existingGameObject, float maxHealth) : base(existingGameObject, maxHealth)
    {
        this.inventory = new Inventory(5,3);
        this.equipmentManager = new EquipmentManager(10, 1);
        this.healthBar = GameObject.Find("PanelHealthBarFill");
        Camera.main.GetComponent<CameraFollow>().SetTargetTransform(existingGameObject.transform);
        GameObject inventoryPanel = GameObject.FindGameObjectWithTag("InventoryPanel");
        inventoryPanel.GetComponent<InventoryPanel>().SetInitialInventory(this.inventory);

        GameObject equipmentPanel = GameObject.FindGameObjectWithTag("EquipmentPanel");
        equipmentPanel.GetComponent<EquipmentPanel>().SetEquipmentManager(this.equipmentManager);

        GameObject hotbarPanel = GameObject.FindGameObjectWithTag("HotbarPanel");
        equipmentPanel.GetComponent<HotbarPanel>().SetEquipmentManager(this.equipmentManager);
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
