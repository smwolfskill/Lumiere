using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Actions/DropItem")]
public class DropItem : EntityAction
{
    private bool lastInput = false;
    //private InventoryBehavior invBehavior = null;
    private InventoryPanel invPanel = null;
    private GameItem toDrop = null;
    private int dropX;
    private int dropY;

    /// <summary>
    /// Checks whether the player can pick up an item from the ground.
    /// </summary>
    /// <param name="obj">The Player's GameObject that wants to execute this action.</param>
    /// <returns>Return false if no item clicked upon, or player not in range of the object.</returns>
    public override bool Validate(GameObject obj)
    {
        bool dropItem = Input.GetKeyDown(SettingsManager.GetDropItem());
        bool inputChanged = dropItem && dropItem != lastInput;
        lastInput = dropItem;
        if(inputChanged)
        {
            GameObject panel = GameObject.FindGameObjectWithTag("InventoryPanel");
            invPanel = panel.GetComponent<InventoryPanel>();
            if(!invPanel.Visible)
            {
                return false;
            }
            else
            {
                dropX = invPanel.SelectedX;
                dropY = invPanel.SelectedY;
                //toDrop = invPanel.ManagedInventory.GetItem(dropX, dropY);
                toDrop = invPanel.GetSelectedItem();
                if (!toDrop.SetYet())
                {
                    return false;
                }
                else
                {
                    return inputChanged;
                }
            }
        }
        else
        {
            return false;
        }
    }

    /// <summary>
    /// Pick up the item.
    /// </summary>
    /// <param name="obj">The GameObject (Player) that wants to execute this action.</param>
    /// <returns>Returns true if the item was picked up successfully, false otherwise.</returns>
    public override bool Execute(GameObject obj)
    {
        bool stackModifierInput = Input.GetKey(SettingsManager.GetStackModifier()); //if pressed, will drop entire stack
        int amountToDrop = 1;
        if(stackModifierInput)
        {
            amountToDrop = toDrop.Quantity;
        }

        GameItem removedItem = invPanel.ManagedInventory.RemoveItem(dropX, dropY, amountToDrop);
        if(removedItem == null)
        {
            return false; //nothing to drop. Inventory bug
        }

        removedItem.CreateGameObject(obj.transform.position);
        return true;
    }
}
