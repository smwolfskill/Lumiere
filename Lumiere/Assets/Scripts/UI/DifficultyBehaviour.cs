using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DifficultyBehaviour : MonoBehaviour 
{

	public UnityEngine.UI.Toggle toggle;
	public Settings.Difficulty difficulty;

	// Use this for initialization
	void Start () 
	{
        toggle.onValueChanged.AddListener(delegate 
        	{
                ToggleValueChanged(toggle);
        	});
        if(!SettingsManager.loaded) //TODO: load settings in main game loop, NOT here.
        {
            SettingsManager.LoadSettings("TODO elsewhere"); //will load default settings since loading from file will fail
        }
        if(SettingsManager.GetDifficulty() == difficulty)
        {
        	toggle.isOn = true;
        }
	}

	void ToggleValueChanged(UnityEngine.UI.Toggle change) 
	{
		if(change.isOn) 
		{
			SettingsManager.SetDifficulty(difficulty);
		}
	}
}
