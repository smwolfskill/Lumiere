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
	private string path;

	/// <summary>
    /// Initialized variables used by navigation buttons on various screens.
    /// </summary>
	void Start() 
	{
		button.onClick.AddListener(OnClick);
		path = "Assets/Resources/settings.txt";
	}

	/// <summary>
    /// Loads the next screen, and executes actions based on which button is clicked.
    /// </summary>
	void OnClick()
	{
		// Deal with settings
		if (button.tag == "UIApplyButton")
		{
			Save(path);
		}
		// Deal with settings
		if (button.tag == "UIRevertButton")
		{
			SettingsManager.LoadSettings(path); //will load default settings since loading from file will fail
			foreach(Button button in settingsButtons)
			{
				button.GetComponent<SettingsButton>().Reload();
			}
			return;
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
	void Save(string file_path)
	{
		SettingsManager.SaveSettings(file_path);
	}

}
