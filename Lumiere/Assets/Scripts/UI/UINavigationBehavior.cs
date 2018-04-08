using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using System;

public class UINavigationBehavior : MonoBehaviour 
{
	public GameObject nextScreen;
	public Button button;
	public GameObject currentScreen;

	// Use this for initialization
	void Start () 
	{
		button.onClick.AddListener(OnClick);
	}
	
	// Update is called once per frame
	void Update () 
	{
		
	}

	void OnClick()
	{
		currentScreen.SetActive(false);
		nextScreen.SetActive(true);
	}
}
