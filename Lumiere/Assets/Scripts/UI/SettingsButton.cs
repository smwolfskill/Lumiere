using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.Experimental.UIElements;

public class SettingsButton : MonoBehaviour
{
    public string settingName;
    public UnityEngine.UI.Button button;
    private bool pressed = false;   //true indicates button has been pressed
    private bool sameClick = false; //true indicates that it's the same instance of Mouse0 being clicked.

    /// <summary>
    /// Initialized variables used by buttons on the SettingsScreen
    /// </summary>
    void Start()
    {
        pressed = false;
        sameClick = false;
        button.onClick.AddListener(OnClick);
        if (!SettingsManager.loaded) //TODO: load settings in main game loop, NOT here.
        {
            SettingsManager.LoadSettings(); //load from default path
        }
        button.GetComponentInChildren<Text>().text = SettingsManager.GetKey(settingName).ToString();
    }

    /// <summary>
    /// Checks if a key was pressed, and if so updates the setting to that key.
    /// </summary>
    void Update()
    {
        if (pressed)
        {
            string key = "";
            bool keyPressed = false;
            Event e = new Event();
            while (Event.PopEvent(e))
            {
                if (e.rawType == EventType.MouseDown)
                {
                    if (sameClick)
                    {
                        sameClick = false;
                    }
                    else
                    {
                        keyPressed = true;
                        key = "Mouse" + e.button;
                        if (e.button == 0)
                        {
                            sameClick = true;
                        }
                        //Debug.Log("Mouse up " + e.button);
                    }
                }
                if (e.rawType == EventType.KeyDown)
                {
                    keyPressed = true;
                    //Debug.Log("KeyCode: " + e.keyCode);
                    key = e.keyCode.ToString();
                    sameClick = false;
                }
            }

            if (keyPressed)
            {
                Debug.Log("Setting key '" + key + "'");
                if (!SettingsManager.SetKey(key, settingName))
                {
                    Debug.Log("Setting failed!");
                }
                pressed = false;
                button.onClick.AddListener(OnClick);
                button.GetComponentInChildren<Text>().text = key;
                return;
            }
        }
    }

    /// <summary>
    /// If the settings button is clicked this updates variables to be used
    /// in the Update function.
    /// </summary>
    void OnClick()
    {
        if (!sameClick)
        {
            //Debug.Log("pressed");
            pressed = true;
            sameClick = true;
            button.onClick.RemoveListener(OnClick); //to allow mouse clicks, disable button interaction until after user presses key
        }
        else
        {
            sameClick = false;
        }
    }

    /// <summary>
    /// Updates the buttons text to the saved setting.
    /// </summary>
    public void Reload()
    {
        button.GetComponentInChildren<Text>().text = SettingsManager.GetKey(settingName).ToString();
    }
}
