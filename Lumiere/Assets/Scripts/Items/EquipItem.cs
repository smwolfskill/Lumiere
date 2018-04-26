using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Actions/EquipItem")]
public class EquipItem : EntityAction
{
    private InventoryPanel invPanel = null;
    private EquipmentPanel eqPanel = null;
    private GameItem toEquip = null;


    /// <summary>
    /// Checks whether the player can equip the item
    /// </summary>
    /// <param name="obj">The Player's GameObject that wants to execute this action.</param>
    /// <returns>Return false if the item cannot be equipped, or if there is no item in the slot.</returns>
    public override bool Validate(GameObject obj)
    {
        bool equipItem = Input.GetButtonDown("EquipItem");
        if (equipItem)
        {
            GameObject UICanvas = GameObject.FindGameObjectWithTag("UICanvas");
            UIBehavior uib = UICanvas.GetComponent<UIBehavior>();
            if (!uib.inventoryPanel.activeSelf)
            {
                return false;
            }
            else
            {
                invPanel = GameObject.FindGameObjectWithTag("InventoryPanel").GetComponent<InventoryPanel>();
                toEquip = invPanel.GetSelectedItem().clone();
                return toEquip != null && (toEquip is EquippableItem || toEquip is UsableItem) && toEquip.SetYet();
            }
        }
        else
        {
            return false;
        }
    }

    /// <summary>
    /// Equips the item.
    /// </summary>
    /// <param name="obj">The GameObject (Player) that wants to execute this action.</param>
    /// <returns>Returns true if the item was equipped successfully, false otherwise.</returns>
    public override bool Execute(GameObject obj)
    {
        invPanel = GameObject.FindGameObjectWithTag("InventoryPanel").GetComponent<InventoryPanel>();
        eqPanel = GameObject.FindGameObjectWithTag("EquipmentPanel").GetComponent<EquipmentPanel>();
        EquipmentManager equipManager = eqPanel.Manager;
        UsableItem useItem = toEquip as UsableItem;
        EquippableItem equipItem = toEquip as EquippableItem;

        if (equipItem != null)
        {
            EquippableItem currentlyEquipped = equipManager.DeEquip(equipItem.Slot);
            Debug.Log(equipItem);
            invPanel.ManagedInventory.RemoveItem(toEquip);
            if (currentlyEquipped == null || invPanel.ManagedInventory.AddItem(currentlyEquipped) == null)
            {
                // eqPanel.Manager = equipManager;
                equipManager.Equip(equipItem); // this should not fail
                return true;
            }
            else
            {
                invPanel.ManagedInventory.AddItem(equipItem);
                equipManager.Equip(currentlyEquipped);
                return false;
            }
        }
        else if (useItem != null)
        {
            bool yay = false;
            for (int i = 0; i < equipManager.GetHotbarItemsSize(); i++)
            {
                yay = equipManager.AddHotBarItem(useItem, i);
                if (yay)
                {
                    invPanel.ManagedInventory.RemoveItem(useItem);
                    break;
                }
            }
            return yay;
        }
        else
        {
            throw new UnityException("WTF are you trying to equip my man");
        }

    }
}
