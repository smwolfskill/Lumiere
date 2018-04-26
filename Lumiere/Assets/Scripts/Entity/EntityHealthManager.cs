using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class EntityHealthManager : MonoBehaviour
{
    public EntityObject entityObj;
    private ParticleSystem damagedParticles;
    private GameObject damageUI;

    void Start()
    {
        damagedParticles = GetComponentInChildren<ParticleSystem>();
        damageUI = GameObject.FindGameObjectWithTag("UIDamage");
    }

    public void InflictDamage(float damageAmount)
    {
        if (entityObj != null)
        {
            if (damagedParticles != null)
            {
                damagedParticles.Play();
                AudioSource damageAudioSource = damagedParticles.GetComponent<AudioSource>();
                if (damageAudioSource != null)
                {
                    damageAudioSource.Play();
                }
            }
            if (damageUI != null)
            {
                Text damageText = damageUI.GetComponent<Text>();
                Animator damageAnimator = damageUI.GetComponent<Animator>();
                damageText.text = ((int)damageAmount).ToString();
                RectTransform damageTransform = damageUI.GetComponent<RectTransform>();
                damageTransform.position = Camera.main.WorldToScreenPoint(transform.position);
                damageAnimator.SetTrigger("Pop");
            }
            entityObj.InflictDamage(damageAmount);

        }
    }
}
