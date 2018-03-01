using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Actions/ToggleInventory")]
public class ToggleInventory : EntityAction
{
    public override bool Validate(GameObject obj)
    {
        //TODO: Replace this in future iterations
        return true;
    }

    public override bool Execute(GameObject obj)
    {
        InventoryBehavior inv = obj.GetComponent<InventoryBehavior>();
        
        if (Input.GetKeyDown(KeyCode.Tab))
        {
            inv.visible = !inv.visible;
        }

        Vector3 mouse = Input.mousePosition;
        inv.mouseX = (int)mouse.x;
        inv.mouseY = Screen.height - (int)mouse.y;

        return true;
    }
}
