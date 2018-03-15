using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EntityHealthManager : MonoBehaviour 
{
    public EntityObject entityObj;

    public void InflictDamage(float damageAmount)
    {
        if (entityObj != null) 
        {
            entityObj.InflictDamage (damageAmount);
        }
    }
}
