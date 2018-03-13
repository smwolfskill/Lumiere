using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Actions/DropItem")]
public class DropItem : EntityAction
{
    private float lastInput = 0.0f;
    private InventoryBehavior invBehavior = null;
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
        float dropItem = Input.GetAxis("DropItem");
        bool inputChanged = dropItem > 0.0 && dropItem != lastInput;
        lastInput = dropItem;
        if(inputChanged)
        {
            invBehavior = obj.GetComponent<InventoryBehavior>();
            if(!invBehavior.Visible)
            {
                return false;
            }
            else
            {
                dropX = invBehavior.SelectedX;
                dropY = invBehavior.SelectedY;
                toDrop = invBehavior.ManagedInventory.GetItem(dropX, dropY);
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
        float stackModifierInput = Input.GetAxis("StackModifier"); //if pressed, will drop entire stack
        int amountToDrop = 1;
        if(stackModifierInput > 0.0)
        {
            amountToDrop = toDrop.Quantity;
        }

        GameItem removedItem = invBehavior.ManagedInventory.RemoveItem(dropX, dropY, amountToDrop);
        if(removedItem == null)
        {
            return false; //nothing to drop. Inventory bug
        }

        GameObject droppedItem = (GameObject) Instantiate(Resources.Load<GameObject>("Prefabs/Item"));
        droppedItem.transform.position = obj.transform.position;
        ItemManager itemManager = droppedItem.GetComponent<ItemManager>();
        itemManager.item = removedItem;
        return true;
    }
}
