using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class UIBehavior : MonoBehaviour
{
    public GameObject inventoryPanel;
    public GameObject tooltip;
    public GameObject uiScreensPanel;
    public GameObject menuPanel;
    public bool inventoryVisible = false;
    public bool menuVisible = false;

    private Canvas canvas;
    private UIScreenManager screenManager = null;
    private float menu_restoreTime = 1.0f;

	// Use this for initialization
	void Start ()
    {
        if(!SettingsManager.loaded) //TODO: load settings in main game loop, NOT here.
        {
            SettingsManager.LoadSettings(); //load from default path
        }
        canvas = GetComponent<Canvas>();
        if(uiScreensPanel != null)
        {
            screenManager = uiScreensPanel.GetComponent<UIScreenManager>();
        }

        //Have UI inputs of greatest precedence (e.g. opening menu) at beginning of conditional 
        //so they will be performed first if there are multiple inputs.
        if(menuVisible)
        {
            ToggleMenu(true);
        }
        else if(inventoryVisible)
        {
            ToggleInventory(true);
        }
        else //no UI windows to show. Hide.
        {
            InventoryPanel panelScript = inventoryPanel.GetComponent<InventoryPanel>();
            panelScript.Reset(); //will initialize Player's inventory
            HideUI();
        }

	}
	
	// Update is called once per frame
    void Update ()
    {
        //Have UI inputs of greatest precedence (e.g. opening menu) at beginning of conditional 
        //so they will be performed first if there are multiple inputs.

        bool toggleMenu = ToggleMenuInput();
        bool toggleInventory = ToggleInventoryInput();
        if(toggleMenu)
        {
            ToggleMenu(!menuVisible);
        }
        else if(toggleInventory && !menuVisible) //don't respond to other input if menu is showing
        {
            ToggleInventory(!inventoryVisible);
        }
	}

    public bool ToggleInventoryInput()
    {
        bool toggleInventoryInput = Input.GetKeyDown(SettingsManager.GetOpenInventory());
        return toggleInventoryInput;
    }

    public void ToggleInventory(bool show)
    {
        if(show)
        {
            ShowUI();
            SetAllInactive();
            inventoryPanel.SetActive(true);
        }
        else
        {
            HideUI();
        }
        inventoryVisible = show;
    }

    public bool ToggleMenuInput()
    {
        bool toggleMenuInput = Input.GetKeyDown(SettingsManager.GetOpenMenu());
        return toggleMenuInput;
    }

    public void ToggleMenu(bool show)
    {
        if(show)
        {
            ShowUI();
            SetAllInactive();
            menuPanel.SetActive(true);
            //Freeze time!
            menu_restoreTime = Time.timeScale;
            Time.timeScale = 0.0f;
        }
        else
        {
            HideUI();
            Time.timeScale = menu_restoreTime;
        }
        menuVisible = show;
    }

    public void SetAllInactive()
    {
        inventoryPanel.SetActive(false);
        tooltip.SetActive(false);
        menuPanel.SetActive(false);
        inventoryVisible = false;
        menuVisible = false;
    }

    public void ShowUI()
    {
        canvas.enabled = true;
    }

    public void HideUI()
    {
        SetAllInactive();
    }

    public void ShowTooltip(string text)
    {
        tooltip.SetActive(true);
        UITooltipBehavior tooltipBehavior = tooltip.GetComponent<UITooltipBehavior>();
        tooltipBehavior.SetText(text);
    }

    public void HideTooltip()
    {
        tooltip.SetActive(false);
    }

    public void GameOver()
    {
        if(uiScreensPanel == null)
        {
            Debug.Log("UIBehavior: Error: No GameOver screen to show; uiScreensPanel is null!");
        } 
        else
        {    
            uiScreensPanel.SetActive(true);
            screenManager.SwitchTo("UIGameOverScreen");
        }
    }
}
