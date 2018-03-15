using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Actions/ToggleInventory")]
public class ToggleInventory : EntityAction
{
    //LEGACY code: toggling inventory is now handled exclusively by UIBehavior in UICanvas,
    //which (will) handle all UI menu open/close toggles.
    private float lastInput = 0.0f;

    public override bool Validate(GameObject obj)
    {
        float toggleInventory = Input.GetAxis("ToggleInventory");
        bool result = toggleInventory > 0.0 && toggleInventory != lastInput;
        lastInput = toggleInventory;
        return result;
    }

    public override bool Execute(GameObject obj)
    {
        InventoryBehavior inv = obj.GetComponent<InventoryBehavior>();
        inv.visible = !inv.visible;
        return true;
    }
}
