using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.EventSystems;
using UnityEngine.Events;

public class EquipmentItemButton : MonoBehaviour, IPointerEnterHandler, IPointerExitHandler
{
    public const float UNSET_ITEM_ALPHA = 30.0f;
    public static Color UNSET_ITEM_COLOR_FG = new Color(1.0f, 1.0f, 1.0f, UNSET_ITEM_ALPHA / 255f); //foreground color
    public static Color UNSET_ITEM_COLOR_BG = new Color(0f, 0f, 0f, UNSET_ITEM_ALPHA / 255f); //background color

    public Button button;
    public EquipmentPanel panelScript;
    public Image backgroundImage;
    public Image itemImage;
    private GameItem item;
    private int locationX;
    private int locationY;

    void Start()
    {
        button.onClick.AddListener(TaskOnClick);
    }

    public void SetGUIReferences(EquipmentPanel panelScript)
    {
        this.panelScript = panelScript;
    }

    public void SetNewItem(GameItem newItem, int locationX, int locationY)
    {
        this.item = newItem;
        this.locationX = locationX;
        this.locationY = locationY;
        backgroundImage.sprite = null;
        itemImage.sprite = newItem.GuiSprite;
        if (item.SetYet())
        {
            backgroundImage.color = newItem.RarityColor();
            itemImage.color = new Color(1f, 1f, 1f, 1f);
        }
        else //UNSET_ITEM is transparent inventory marking
        {
            backgroundImage.color = UNSET_ITEM_COLOR_BG;
            itemImage.color = UNSET_ITEM_COLOR_FG;
        }
    }

    void TaskOnClick()
    {
        //TODO: Do anything on item click?
        //Debug.Log("TODO: anything on item click?");
    }

    public void OnPointerEnter(PointerEventData eventData)
    {
        panelScript.SetSelected(locationX, locationY);
    }

    public void OnPointerExit(PointerEventData eventData)
    {
        panelScript.ResetSelectedItem();
    }
}
