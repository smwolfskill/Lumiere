using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class ExitButton : MonoBehaviour 
{
    public Button button;
	
	void Start()
    {
        button.onClick.AddListener(OnClick);
	}

    void OnClick()
    {
        Application.Quit();
        Debug.Log("Exiting game...");
        UnityEditor.EditorApplication.isPlaying = false;
    }

}
