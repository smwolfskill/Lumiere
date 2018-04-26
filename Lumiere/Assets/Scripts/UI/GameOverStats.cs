using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using System;

public class GameOverStats : MonoBehaviour
{
    public Text statsText;
	
    public void SetStats(int dungeonsExplored)
    {
        if(!SettingsManager.loaded)
        {
            SettingsManager.LoadSettings();
        }
        string difficulty = SettingsManager.GetDifficulty().ToString();
        statsText.text = "<b>Difficulty</b>: " + difficulty + Environment.NewLine
                       + "<b>Dungeons Explored</b>: " + dungeonsExplored.ToString();
    }
}
