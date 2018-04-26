using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// A utility class that can be attached to GameObjects for timing purposes.
/// </summary>
public class Timer : MonoBehaviour 
{
    private float timer;
    private bool tEnabled;

	// Use this for initialization
	void Start () 
    {
        tEnabled = false;
        timer = 0f;
	}
	
	// Update is called once per frame
	void Update () 
    {
        if (tEnabled) 
        {
            timer += Time.deltaTime;    
        }
	}

    public void Reset()
    {
        timer = 0f;
        tEnabled = true;
    }

    public bool HasExceeded(float limit)
    {
        return timer >= limit;
    }

    public bool Enabled 
    {
        get 
        {
            return tEnabled;
        }
    }
}
