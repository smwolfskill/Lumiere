using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class DifficultyBehaviour : MonoBehaviour
{

    public UnityEngine.UI.Toggle toggle;
    public Settings.Difficulty difficulty;

    /// <summary>
    /// Sets up variables used by the difficulty toggles
    /// </summary>
    void Start()
    {
        toggle.onValueChanged.AddListener(delegate
            {
                ToggleValueChanged(toggle);
            });
        if (!SettingsManager.loaded) //TODO: load settings in main game loop, NOT here.
        {
            SettingsManager.LoadSettings("TODO elsewhere"); //will load default settings since loading from file will fail
        }
        if (SettingsManager.GetDifficulty() == difficulty)
        {
            toggle.isOn = true;
        }
    }

    /// <summary>
    /// Sets the difficulty when the toggle is turned on.
    /// </summary>
    void ToggleValueChanged(UnityEngine.UI.Toggle change)
    {
        if (change.isOn)
        {
            SettingsManager.SetDifficulty(difficulty);
        }
    }
}
