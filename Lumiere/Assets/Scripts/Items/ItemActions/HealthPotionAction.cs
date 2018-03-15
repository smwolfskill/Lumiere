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
        //TODO: modify player's PlayerObject health once this it4_items branch is merged with it4_MonsterAI
        //player.entityObject.ModifyHealth(10.0f);
        Debug.Log("Health potion used");
        return true;
    }
}
