using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using System;
using UnityEngine.SceneManagement;

public class UINavigationBehavior : MonoBehaviour 
{
	public GameObject[] nextScreens;
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
            case "UIGameOverButton": //reload the scene to ensure the game is entirely reset.
                /*GameObject player = GameObject.FindGameObjectWithTag("Player");
                if(player != null)
                {
                Debug.Log("player exists");
                    Animator anim = player.GetComponent<Animator>();
                    //PlayerObject playerObject = player.GetComponent<EntityHealthManager>().entityObj;
                    anim.SetTrigger("TDie");
                    GameObject.Destroy(player, 1f);
                }*/
                Scene scene = SceneManager.GetActiveScene(); //current scene
                SceneManager.LoadScene(scene.name, LoadSceneMode.Single);
                break;
        }

        if(nextScreens == null)
		{
            //Hide all current screens
            foreach(GameObject screen in currentScreens)
            {
                screen.SetActive(false);
            }
		}
		else
		{
            //1. Hide current screens if one of next screens is the settings screen
            bool nextIsSettingsScreen = false;
            foreach(GameObject screen in nextScreens)
            {
                if(screen.tag == "UISettingsScreen")
                {
                    nextIsSettingsScreen = true;
                    break;
                }
            }
            if(!nextIsSettingsScreen)
            {
                //Hide all current screens, unless going to settings screen
                foreach(GameObject screen in currentScreens)
                {
                    screen.SetActive(false);
                }
            }

            //2. Show next screens
            foreach(GameObject screen in nextScreens)
            {
                screen.SetActive(true);
            }
			//nextScreen.SetActive(true);
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
