using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using System;

public class UINavigationBehavior : MonoBehaviour 
{
	public GameObject nextScreen;
	public Button button;
	public Button[] settingsButtons;
	public GameObject[] currentScreens;

	/// <summary>
    /// Initialized variables used by navigation buttons on various screens.
    /// </summary>
	void Start() 
	{
		button.onClick.AddListener(OnClick);
	}

	/// <summary>
    /// Loads the next screen, and executes actions based on which button is clicked.
    /// </summary>
	void OnClick()
	{
        //Deal with settings if applicable
        switch(button.tag)
        {
            case "UIApplyButton":
                Save();
                break;
            case "UIRevertButton":
                LoadSettingsToButtons();
                return; //don't close any windows; stay on the Settings screen
            case "UICancelButton": //same as revert, but exits Settings screen after clicking
                LoadSettingsToButtons();
                break;
        }

		if(nextScreen == null || (nextScreen != null && nextScreen.tag != "UISettingsScreen"))
		{
            //Hide all current screens
            foreach(GameObject screen in currentScreens)
            {
                screen.SetActive(false);
            }
		}
		if(nextScreen != null)
		{
            //Show next screen
			nextScreen.SetActive(true);
		}

		
	}

	/// <summary>
    /// Saves the settings to a given file.
    /// </summary>
    /// <param name="file_path">The string containing the path to the file that 
    /// settings should be written to.</param>
    /// <returns>Returns true if writing to the file was successful.</returns>
    void Save(string file_path = SettingsManager.DEFAULT_PATH)
	{
		SettingsManager.SaveSettings(file_path);
	}

    void LoadSettingsToButtons(string file_path = SettingsManager.DEFAULT_PATH)
    {
        SettingsManager.LoadSettings(file_path); //will load old, already saved settings
        foreach(Button button in settingsButtons)
        {
            button.GetComponent<SettingsButton>().Reload();
        }
    }

}
