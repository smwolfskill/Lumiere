using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

/// <summary>
/// Front-end implementation of the equipment GUI
/// </summary>


public class EquipmentPanel : MonoBehaviour
{
    public Entity entity;

    [Header("General Settings")]
    public bool visible;
    public int blockSize;
    public int padding;
    public Color background;
    public Color selected;

    [Header("Equipment Settings")]
    public int nWidth;
    public int nHeight;
    public int offsetX;
    public int offsetY;

    [Header("Equipment Items")]
    public string itemUIPrefabLocation = "EquipmentItemButton";
    public int numberOfItems = 6;
    public Sprite item1;
    public Sprite item2;
    public Sprite item3;
    public Sprite item4;
    public Sprite item5;
    public Sprite item6;

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

    public EquipmentManager Manager
    {
        get 
        {
            return equipmentManager;
        }

        set 
        {
            equipmentManager = value;
            Initialize ();
        }
    }
    #endregion

    void Start ()
    {

        //if(!initialized)
        //{
        //    Reset();
        //}
    }


    public void Initialize()
    {
        equipmentManager.equipPanel = this;
        gridLayout = GetComponent<GridLayoutGroup>();
        //entity.equipmentManager = equipmentManager;
        ResetSelectedItem();
        DrawEquipment();
        initialized = true;
    }

    public void Reset()
    {
        //inv = new Inventory(nWidth, nHeight);
        //Initialize();
    }

    public void DrawEquipment()
    {
        int equippableItemsSize = equipmentManager.GetEquipmentSize();
        nWidth = 1;
        nHeight = equippableItemsSize;
        gridLayout.constraintCount = nWidth; //# of columns
        capacity = nWidth * nHeight;
        EquipmentItemButton[] childrenScripts = GetComponentsInChildren<EquipmentItemButton>(); //used to access the children game objects
        //Set or create each GameItem's UI element
        //TODO: understand how/why duplicate 
        for(int location = 0; location < capacity; location++)
        {
            int[] gridLocation = ItemLocationInGrid(location);
            int locX = gridLocation[0];
            int locY = gridLocation[1];
            EquipmentItemButton childScript;
            if(location >= childrenScripts.Length) //Equipment panel grew in size; create more UI items.
            {
                GameObject child = (GameObject) GameObject.Instantiate(Resources.Load<GameObject>("Prefabs/" + itemUIPrefabLocation));
                child.transform.SetParent(gameObject.transform, false);
                childScript = child.GetComponent<EquipmentItemButton>();
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
                
            GameItem tmpItem = equipmentManager.GetEquippedItemFromIndex(locY);
            //TODO is this something to fix?
            if (tmpItem == null)
            {
                tmpItem = GameItem.UNSET_ITEM;
            }
            childScript.SetNewItem(tmpItem, locX, locY);
        }

        //If equipment panel shrank in size, delete extra UI slot elements
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
            return equipmentManager.GetEquippedItemFromIndex(selectedY);
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

    //TODO do we need to remove this?
    /// <summary>
    /// Uses Item action(s) of a selected item in the inventory, if valid.
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