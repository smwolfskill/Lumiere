﻿using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class UIBehavior : MonoBehaviour
{
    public GameObject inventoryPanel;
    public GameObject tooltip;
    public bool inventoryVisible = false;

    private float lastToggleInventoryInput = 0.0f; //last input to toggle inventory
    private Canvas canvas;

	// Use this for initialization
	void Start ()
    {
        canvas = GetComponent<Canvas>();
        //TODO in future iterations: add other methods of input which would open/close UI (e.g. Esc to open the menu).
        //Have UI inputs of greatest precedence (e.g. opening menu) at beginning of conditional 
        //so they will be performed last if there are multiple inputs.
        /*if(menuVisible)
        {
            ToggleMenu(true);
        }

        else */if(inventoryVisible)
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
        //TODO in future iterations: add other methods of input which would open/close UI (e.g. Esc to open the menu).
        //Have UI inputs of greatest precedence (e.g. opening menu) at beginning of conditional 
        //so they will be performed last if there are multiple inputs.

        bool toggleInventory = ToggleInventoryInput();
        /*if(toggleMenu) //fill-in once have menu UI
        {
            ToggleMenu(!menuVisible);
        }
        else */if(toggleInventory)
        {
            ToggleInventory(!inventoryVisible);
        }
	}

    public bool ToggleInventoryInput()
    {
        float toggleInventoryInput = Input.GetAxis("ToggleInventory");
        bool toggleInventory = toggleInventoryInput > 0.0f && toggleInventoryInput != lastToggleInventoryInput;
        lastToggleInventoryInput = toggleInventoryInput;
        return toggleInventory;
    }

    public void ToggleInventory(bool show)
    {
        inventoryVisible = show;
        if(inventoryVisible)
        {
            ShowUI();
            SetAllInactive();
            inventoryPanel.SetActive(true);
        }
        else
        {
            HideUI();
        }
    }

    public void SetAllInactive()
    {
        inventoryPanel.SetActive(false);
        tooltip.SetActive(false);
        //TODO in future iterations: set all separate UI components to be inactive.
    }

    public void ShowUI()
    {
        canvas.enabled = true;
    }

    public void HideUI()
    {
        //canvas.enabled = false;
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
}
