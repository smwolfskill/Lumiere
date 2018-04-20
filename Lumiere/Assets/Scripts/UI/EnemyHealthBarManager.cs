using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EnemyHealthBarManager : MonoBehaviour
{
    private float initialScale;

	// Use this for initialization
	void Start ()
    {
        initialScale = transform.localScale.y;
	}
	
    public void SetHealth(float percent)
    {
        float scaleToSet = percent * initialScale;
        transform.localScale = new Vector3(transform.localScale.x, scaleToSet, transform.localScale.z);
    }
}
