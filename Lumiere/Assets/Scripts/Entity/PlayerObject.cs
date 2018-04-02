using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PlayerObject : EntityObject
{
    private GameObject healthBar;

    public Dictionary<EquippableItem.EquipmentSlot, GameItem> Equipment
    {
        get; private set;
    }

    public PlayerObject(GameObject existingGameObject, float maxHealth) : base(existingGameObject, maxHealth)
    {
        this.inventory = new Inventory(5,3);
        this.healthBar = GameObject.Find("PanelHealthBarFill");
        this.Equipment = new Dictionary<EquippableItem.EquipmentSlot, GameItem>();
        Camera.main.GetComponent<CameraFollow>().SetTargetTransform(existingGameObject.transform);
        GameObject panel = GameObject.FindGameObjectWithTag("InventoryPanel");
        panel.GetComponent<InventoryPanel>().SetInitialInventory(this.inventory);

        // initialize everything to null, so this should be a full Dictionary
        foreach (EquippableItem.EquipmentSlot slot in
            Enum.GetValues(typeof(EquippableItem.EquipmentSlot)))
        {
            this.Equipment[slot] = null;
        }
    }

    public override void InflictDamage(float damageAmount)
    {
        base.InflictDamage(damageAmount);
        UpdateHealthBar();
    }

    public override void Heal(float healAmount)
    {
        base.Heal(healAmount);
        UpdateHealthBar();
    }

    /// <summary>
    /// Equips the item in the specified inventory slot
    /// </summary>
    /// <param name="x">inventory x</param>
    /// <param name="y">inventory y</param>
    /// <returns>true if the item was equipped</returns>
    public bool Equip(int x, int y)
    {
        GameItem item = this.inventory.RemoveItem(x, y, 1);
        EquippableItem toEquip = item as EquippableItem;
        if (toEquip != null)
        {
            toEquip.Quantity = 1;
            if (this.Equipment[toEquip.EquipSlot] == null ||
                this.inventory.AddItem(Equipment[toEquip.EquipSlot]) == null)
            {
                // AddItem returns null if everything is added
                this.Equipment[toEquip.EquipSlot] = toEquip;
                return true;
            }
            else
            {
                // Inventory full, put the item we tried to equip back into the
                // inventory. If we can't add it back, then something with the
                // stack system is broken.
                this.inventory.AddItem(toEquip);
                return false;
            }
        }
        else
        {
            // We didn't get an equippable item
            return false;
        }
    }

    /// <summary>
    /// Tries to unequip an item given an equipment slot
    /// </summary>
    /// <param name="slot"></param>
    /// <returns>true if the equipment slot has been cleared</returns>
    public bool Unequip(EquippableItem.EquipmentSlot slot)
    {
        if (this.Equipment[slot] == null)
        {
            return true;
        }
        else
        {
            if (this.inventory.AddItem(this.Equipment[slot]) == null)
            {
                // clear out the equipment slot
                this.Equipment[slot] = null;
                return true;
            }
            else
            {
                return false;
            }
        }
    }

    private void UpdateHealthBar()
    {
        this.healthBar.GetComponent<HealthBarManager>().SetHealth(currHealth / maxHealth);
    }
}
