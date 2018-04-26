using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class EntityAction : Action
{
    protected Entity target;
    public AudioClip actionClip;

    public override bool Execute(GameObject obj)
    {
        AudioSource source = obj.GetComponent<AudioSource>();
        if (source != null && actionClip != null)
        {
            source.PlayOneShot(actionClip);
        }

        return true;
    }
}
