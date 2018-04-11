using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Actions/DropItem")]
public class DropItem : EntityAction
{
    private InventoryPanel invPanel = null;
    private GameItem toDrop = null;

    /// <summary>
    /// Checks whether the player can drop an item on the ground.
    /// </summary>
    /// <param name="obj">The Player's GameObject that wants to execute this action.</param>
    /// <returns>Return false if the item cannot be dropped, or if there is no item in the slot.</returns>
    public override bool Validate(GameObject obj)
    {
        bool dropItem = Input.GetKeyDown(SettingsManager.GetDropItem());
        if(dropItem)
        {
            GameObject panel = GameObject.FindGameObjectWithTag("InventoryPanel");
            invPanel = panel.GetComponent<InventoryPanel>();
            if(!invPanel.Visible)
            {
                return false;
            }
            else
            {
                toDrop = invPanel.GetSelectedItem();
                if (!toDrop.SetYet())
                {
                    return false;
                }
                else
                {
                    return true;
                }
            }
        }
        else
        {
            return false;
        }
    }

    /// <summary>
    /// Drop the item.
    /// </summary>
    /// <param name="obj">The GameObject (Player) that wants to execute this action.</param>
    /// <returns>Returns true if the item was dropped successfully, false otherwise.</returns>
    public override bool Execute(GameObject obj)
    {
        bool stackModifierInput = Input.GetKey(SettingsManager.GetStackModifier()); //if pressed, will drop entire stack
        int amountToDrop = 1;
        if(stackModifierInput)
        {
            amountToDrop = toDrop.Quantity;
        }

        GameItem removedItem = invPanel.ManagedInventory.RemoveItem(invPanel.SelectedX, invPanel.SelectedY, amountToDrop);
        if(removedItem == null)
        {
            return false; //nothing to drop. Inventory bug
        }

        removedItem.CreateGameObject(obj.transform.position);
        return true;
    }
}
