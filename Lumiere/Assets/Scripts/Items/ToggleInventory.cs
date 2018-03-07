using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Actions/ToggleInventory")]
public class ToggleInventory : EntityAction
{
    private float lastInput = 0.0f;

    public override bool Validate(GameObject obj)
    {
        //TODO: Replace this in future iterations
        return true;
    }

    public override bool Execute(GameObject obj)
    {
        InventoryBehavior inv = obj.GetComponent<InventoryBehavior>();

        float toggleInventory = Input.GetAxis("ToggleInventory");
        if (toggleInventory > 0.0 && toggleInventory != lastInput)
        {
            inv.visible = !inv.visible;
        }

        Vector3 mouse = Input.mousePosition;
        inv.mouseX = (int)mouse.x;
        inv.mouseY = Screen.height - (int)mouse.y;
        lastInput = toggleInventory;
        return true;
    }
}
