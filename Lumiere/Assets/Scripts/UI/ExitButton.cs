using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class ExitButton : MonoBehaviour
{
    public Button button;

    /// <summary>
    /// Adds the function to be executed when the ExitButton is clicked
    /// </summary>
    void Start()
    {
        button.onClick.AddListener(OnClick);
    }

    /// <summary>
    /// Closes the application.
    /// </summary>
    void OnClick()
    {
        Application.Quit();
        //Quit the game if running in Unity Editor. Comment out to build correctly for deployment.
        //(references to UnityEditor not allowed!)
        /*Debug.Log("Exiting game...");
        UnityEditor.EditorApplication.isPlaying = false;*/
    }

}
