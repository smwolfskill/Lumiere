using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class ItemAction : EntityAction 
{
    public int itemID;

    protected float lastInput = 0.0f;
    protected InventoryPanel invPanel;
    protected GameItem toUse;
    protected int useX;
    protected int useY;

    public bool UseItemInput()
    {
        float useItemInput = Input.GetAxis("UseItem");
        bool inputChanged = useItemInput > 0.0 && useItemInput != lastInput;
        lastInput = useItemInput;
        return inputChanged;
    }

    /// <summary>
    /// Checks whether the player is selecting the item to be activated in their inventory.
    /// </summary>
    /// <param name="obj">The Player's GameObject that wants to execute this action.</param>
    /// <returns>Return false if no item clicked upon, or player not in range of the object.</returns>
    public override bool Validate(GameObject obj)
    {
        if(UseItemInput())
        {
            //Debug.Log("use item input");
            GameObject panel = GameObject.FindGameObjectWithTag("InventoryPanel");
            invPanel = panel.GetComponent<InventoryPanel>();
            if(!invPanel.Visible)
            {
                return false;
            }
            else
            {
                useX = invPanel.SelectedX;
                useY = invPanel.SelectedY;
                toUse = invPanel.GetSelectedItem();
                if (toUse.ItemID != itemID || !toUse.SetYet())
                {
                    return false;
                }
                else
                {
                    return true;
                }
            }
        }
        return false;
    }

    public override bool Execute(GameObject obj)
    {
        if(invPanel == null)
        {
            return false;
        }
        GameItem removedItem = invPanel.ManagedInventory.RemoveItem(useX, useY, 1);
        return true;
    }
}
