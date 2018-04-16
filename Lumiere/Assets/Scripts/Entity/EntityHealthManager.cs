using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EntityHealthManager : MonoBehaviour 
{
    public EntityObject entityObj;
    private ParticleSystem damagedParticles;

    void Start()
    {
        damagedParticles = GetComponentInChildren<ParticleSystem>();
    }

    public void InflictDamage(float damageAmount)
    {
        if (entityObj != null) 
        {
            entityObj.InflictDamage (damageAmount);
            if(damagedParticles != null)
            {
                damagedParticles.Play();
            }
        }
    }
}
