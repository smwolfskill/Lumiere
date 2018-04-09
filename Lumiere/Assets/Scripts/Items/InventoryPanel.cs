using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class InventoryPanel : MonoBehaviour
{
    public UIBehavior uiBehavior;
    public Entity entity;

    [Header("General Settings")]
    public bool visible;
    public int blockSize;
    public int padding;
    public Color background;
    public Color selected;

    [Header("Inventory Settings")]
    public int nWidth;
    public int nHeight;
    public int offsetX;
    public int offsetY;

    [Header("Inventory Items")]
    public string itemUIPrefabLocation = "InventoryItemButton";
    public int numberOfItems = 5;
    public Sprite item1;
    public Sprite item2;
    public Sprite item3;
    public Sprite item4;
    public Sprite item5;
    public Sprite item6;
    public Sprite item7;
    public Sprite item8;

    //mouse input
    private int mouseX;
    private int mouseY;
    private int selectedX;
    private int selectedY;

    // Privately managed inventory
    private Inventory inv;
    private GridLayoutGroup gridLayout;
    private int capacity;
    private bool initialized = false;

    #region Getters & Setters
    public Inventory ManagedInventory
    {
        get
        {
            return inv;
        }
    }

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

    public void SetInitialInventory(Inventory playerInventory)
    {
        inv = playerInventory;
        AddInitialItems();
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
        inv.uiPanel = this;
        gridLayout = GetComponent<GridLayoutGroup>();
        entity.inventory = inv;
        ResetSelectedItem();
        inv.UpdateUI();
        initialized = true;
    }

    public void AddInitialItems(){
        inv.AddItem(new GameItem(item1, item1, "item 1", "item 1", 50,
            GameItem.ItemRarity.COMMON, 5, 64, 1));
        inv.AddItem(new GameItem(item2, item2, "item 2", "item 2", 40,
            GameItem.ItemRarity.UNCOMMON, 4, 64, 2));
        inv.AddItem(new GameItem(item3, item3, "item 3", "item 3", 30,
            GameItem.ItemRarity.RARE, 3, 64, 3));
        inv.AddItem(new UsableItem(item4, item4, "Health Potion", "A health potion.", 1,
            GameItem.ItemRarity.EPIC, 2, 5, 100, "HealthPotionAction"));
        inv.AddItem(new GameItem(item5, item5, "The God Portal", "The ultimate portal of mysticality and memes.", 10,
            GameItem.ItemRarity.LEGENDARY, 1, 64, 5));
        inv.AddItem(new WeaponItem(item6, item6, "Battleaxe", "This Dwarven axe was forged in the Mines of Moria", 1, 
            GameItem.ItemRarity.COMMON, 1, 1, 6, null, 5, 1));
        inv.AddItem(new ArmorItem(item7, item7, "Chainmail", "An iron chainmail that offers basic protection", 1, 
            GameItem.ItemRarity.COMMON, 1, 1, 7, EquipmentManager.EquipSlot.CHEST, 10, 2, 1));
        inv.AddItem(new ArmorItem(item8, item8, "Spooky mask", "Why would you wear this horrendous mask?", 1, 
            GameItem.ItemRarity.RARE, 1, 1, 8, EquipmentManager.EquipSlot.HEAD, 2, 2, 1));
    }

    public void Reset()
    {
        //inv = new Inventory(nWidth, nHeight);
        //Initialize();
    }

    public void DrawInventory()
    {
        nWidth = inv.GetWidth();
        nHeight = inv.GetHeight();
        capacity = nWidth * nHeight;
        Debug.Log(capacity.ToString());
        gridLayout.constraintCount = nWidth; //# of columns
        InventoryItemButton[] childrenScripts = GetComponentsInChildren<InventoryItemButton>(); //used to access the children game objects
        //Set or create each GameItem's UI element
        //TODO: understand how/why duplicate 
        for(int location = 0; location < capacity; location++)
        {
            int[] gridLocation = ItemLocationInGrid(location);
            int locX = gridLocation[0];
            int locY = gridLocation[1];
            InventoryItemButton childScript;
            if(location >= childrenScripts.Length) //Inventory grew in size; create more UI items.
            {
                GameObject child = (GameObject) GameObject.Instantiate(Resources.Load<GameObject>("Prefabs/" + itemUIPrefabLocation)/*,
                                                 gameObject.transform*/);
                child.transform.SetParent(gameObject.transform, false);
                childScript = child.GetComponent<InventoryItemButton>();
                childScript.SetGUIReferences(uiBehavior, this);
            }
            else
            {
                childScript = childrenScripts[location];
            }

            if(locX < 0 || locX >= nWidth || locY < 0 || locY >= nHeight)
            {
                Debug.Log("Set new item wrong params: (" + locX + "/ " + nWidth + ", " + locY + "/ " + nHeight + ")");
            }
            childScript.SetNewItem(inv.GetItem(locX, locY), locX, locY);
        }

        //If inventory shrank in size, delete extra UI slot elements
        for(int overIndex = capacity; overIndex < childrenScripts.Length; overIndex++)
        {
            GameObject.Destroy(childrenScripts[overIndex].gameObject);
        }
    }

    public void UpdateItemQuantityText(int locationX, int locationY)
    {
        InventoryItemButton[] childrenScripts = GetComponentsInChildren<InventoryItemButton>(); //used to access the children game objects
        childrenScripts[locationX + locationY * nWidth].UpdateQuantityText();
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
            return inv.GetItem(selectedX, selectedY);            
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