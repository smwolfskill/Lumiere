using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using System;

public class UINavigationBehavior : MonoBehaviour 
{
	public GameObject nextScreen;
	public Button button;
	public GameObject[] currentScreens;


	void Start() 
	{
		button.onClick.AddListener(OnClick);
	}

	void OnClick()
	{
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

}
