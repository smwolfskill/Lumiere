using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;

public class EntityHealthManager : MonoBehaviour 
{
    public EntityObject entityObj;

    public void InflictDamage(float damageAmount)
    {
        if (entityObj != null) 
        {
        	EquipmentManager equipmentManager = entityObj.GetComponent<EquipmentManager> ();
        	if (equipmentManager != null)
        	{
        		double armorRating = 0;
        		armorRating = equipmentManager.GetArmorRating();
        		Console.Debug(armorRating);
        	}
            entityObj.InflictDamage (damageAmount);
        }
    }
}
