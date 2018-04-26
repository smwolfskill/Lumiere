using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;

public class UIScreenManager : MonoBehaviour
{
    public GameObject[] screens;

    /// <summary>
    /// Switches to the screen whose tag matches screenTag, or throws an ArgumentException if not found.
    /// </summary>
    /// <param name="screenTag">Tag of screen GameObject to switch to (set active).</param>
    public void SwitchTo(string screenTag)
    {
        bool found = false;
        foreach (GameObject screen in screens)
        {
            if (screen.tag == screenTag)
            {
                found = true;
                screen.SetActive(true);
            }
            else
            {
                screen.SetActive(false);
            }
        }
        if (!found)
        {
            throw new ArgumentException("UIScreenManager: tag '" + screenTag + "' did not match any screens!");
        }
    }
}
