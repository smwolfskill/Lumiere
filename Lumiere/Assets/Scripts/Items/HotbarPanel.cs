using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

/// <summary>
/// Front-end implementation of the hotbar GUI.
/// </summary>


public class HotbarPanel : MonoBehaviour
{
    public Entity entity;

    [Header("General Settings")]
    public bool visible;
    public int blockSize;
    public int padding;
    public Color background;
    public Color selected;

    [Header("Hotbar Settings")]
    public int nWidth;
    public int nHeight;
    public int offsetX;
    public int offsetY;

    [Header("Hotbar Items")]
    public string itemUIPrefabLocation = "HotbarItemButton";
    public int numberOfItems = 10;
    public Sprite item01;
    public Sprite item02;
    public Sprite item03;
    public Sprite item04;
    public Sprite item05;
    public Sprite item06;
    public Sprite item07;
    public Sprite item08;
    public Sprite item09;
    public Sprite item10;

    //mouse input
    private int mouseX;
    private int mouseY;
    private int selectedX;
    private int selectedY;


    private EquipmentManager equipmentManager;
    private GridLayoutGroup gridLayout;
    private int capacity;
    private bool initialized = false;

    #region Getters & Setters
    public int SelectedX
    {
        get
        {
            return selectedX;
        }
    }

    public int SelectedY
    {
        get
        {
            return selectedY;
        }
    }

    public bool Visible {
        get
        {
            return visible;
        }
    }
    #endregion

    public void SetEquipmentManager(EquipmentManager playerEquipmentManager)
    {
        equipmentManager = playerEquipmentManager;
        Initialize();
    }

    void Start ()
    {

        //if(!initialized)
        //{
        //    Reset();
        //}
    }


    public void Initialize()
    {
        equipmentManager.hotbarPanel = this;
        gridLayout = GetComponent<GridLayoutGroup>();
        //entity.equipmentManager = equipmentManager;
        ResetSelectedItem();
        DrawHotbar();
        initialized = true;
    }

    public void Reset()
    {
        //inv = new Inventory(nWidth, nHeight);
        //Initialize();
    }

    public void DrawHotbar()
    {
        int usableItemsSize = equipmentManager.GetHotbarItemsSize();
        nWidth = usableItemsSize;
        nHeight = 1;
        gridLayout.constraintCount = nWidth; //# of columns
        capacity = nWidth * nHeight;
        HotbarItemButton[] childrenScripts = GetComponentsInChildren<HotbarItemButton>(); //used to access the children game objects
        //Set or create each GameItem's UI element
        //TODO: understand how/why duplicate 
        for(int location = 0; location < capacity; location++)
        {
            int[] gridLocation = ItemLocationInGrid(location);
            int locX = gridLocation[0];
            int locY = gridLocation[1];
            HotbarItemButton childScript;
            if(location >= childrenScripts.Length) //Hotbar grew in size; create more UI items.
            {
                GameObject child = (GameObject) GameObject.Instantiate(Resources.Load<GameObject>("Prefabs/" + itemUIPrefabLocation));
                child.transform.SetParent(gameObject.transform, false);
                childScript = child.GetComponent<HotbarItemButton>();
                childScript.SetGUIReferences(this);
            }
            else
            {
                childScript = childrenScripts[location];
            }

            if(locX < 0 || locX >= nWidth || locY < 0 || locY >= nHeight)
            {
                Debug.Log("Set new item wrong params: (" + locX + "/ " + nWidth + ", " + locY + "/ " + nHeight + ")");
            }

            GameItem tmpItem = equipmentManager.GetHotbarItem(locX);
            //TODO is this something to fix?
            if (tmpItem == null)
            {
                tmpItem = GameItem.UNSET_ITEM;
            }
            childScript.SetNewItem(tmpItem, locX, locY);
        }

        //If hotbar shrank in size, delete extra UI slot elements
        for(int overIndex = capacity; overIndex < childrenScripts.Length; overIndex++)
        {
            GameObject.Destroy(childrenScripts[overIndex].gameObject);
        }
    }

    void Update()
    {
        Vector3 mouse = Input.mousePosition;
        mouseX = (int)mouse.x;
        mouseY = Screen.height - (int)mouse.y;

        UseItemActions();
    }

    public GameItem GetSelectedItem()
    {
        if (visible == false || selectedX < 0 || selectedY < 0) {
            return null;
        }
        else
        {
            Debug.Log(equipmentManager);
            return equipmentManager.GetHotbarItem(selectedX);
        }
    }

    public void SetSelected(int locationX, int locationY)
    {
        selectedX = locationX;
        selectedY = locationY;
    }

    public void ResetSelectedItem()
    {
        selectedX = -1;
        selectedY = -1;
    }
        
    /// <summary>
    /// Uses Item action(s) of a selected item in the hotbar, if valid.
    /// </summary>
    public void UseItemActions()
    {
        GameItem selectedItem = GetSelectedItem();

        if (selectedItem == null)
            return;

        if (selectedItem is UsableItem)
        {
            UsableItem selectedUsable = (UsableItem)selectedItem;
            if (selectedUsable.ValidateUse(gameObject))
            {
                //Player selected to use the current item, and it is valid to be used
                selectedUsable.Use(gameObject);
            }
        }
    }

    /// <summary>
    /// Convert 1D item location to 2D [x, y] coord. in grid.
    /// </summary>
    /// <returns>The location in grid.</returns>
    /// <param name="locationID">Location I.</param>
    public int[] ItemLocationInGrid(int locationID)
    {
        return new int[] {locationID % nWidth, locationID / nWidth};
    }
}