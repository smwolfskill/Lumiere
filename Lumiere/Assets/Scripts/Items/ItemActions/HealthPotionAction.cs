using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Actions/ItemAction/HealthPotionAction")]
public class HealthPotionAction : ItemAction
{
    /// <summary>
    /// Executes this action for the specified GameObject.
    /// </summary>
    /// <param name="obj">The GameObject that wants to execute this action.</param>
    /// <returns>Returns true if this action is executed successfully, false otherwise.</returns>
    public override bool Execute(GameObject obj)
    {
        bool baseSuccess = base.Execute(obj);
        if(!baseSuccess)
        {
            return false;
        }
        Entity player = invPanel.entity;
        //TODO: make the heal amount more customizable in the future (for different strength potions)
        player.entityObject.Heal(10.0f);
        //Debug.Log("Health potion used");
        return true;
    }
}
