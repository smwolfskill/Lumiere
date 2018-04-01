using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Actions/EquipItem")]
public class EquipItem : EntityAction
{
    private InventoryPanel invPanel = null;
    private GameItem toEquip = null;


    /// <summary>
    /// Checks whether the player can equip the item
    /// </summary>
    /// <param name="obj">The Player's GameObject that wants to execute this action.</param>
    /// <returns>Return false if the item cannot be equipped, or if there is no item in the slot.</returns>
    public override bool Validate(GameObject obj)
    {
        bool equipItem = Input.GetButtonDown("EquipItem");
        if(equipItem)
        {
            GameObject panel = GameObject.FindGameObjectWithTag("InventoryPanel");
            invPanel = panel.GetComponent<InventoryPanel>();
            if(!invPanel.Visible)
            {
                return false;
            }
            else
            {
                toEquip = invPanel.GetSelectedItem();
                if (!toEquip.SetYet())
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
    /// Equips the item.
    /// </summary>
    /// <param name="obj">The GameObject (Player) that wants to execute this action.</param>
    /// <returns>Returns true if the item was equipped successfully, false otherwise.</returns>
    public override bool Execute(GameObject obj)
    {
        return false;
    }
}
