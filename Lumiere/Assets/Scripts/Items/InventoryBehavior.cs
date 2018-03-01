using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class InventoryBehavior : MonoBehaviour {
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

    [Header("Inventory Rarity Settings")]
    public Color colorCommon;
    public Color colorUncommon;
    public Color colorRare;
    public Color colorEpic;
    public Color colorLegendary;

    [Header("Inventory Items")]
    public Sprite item1;
    public Sprite item2;
    public Sprite item3;
    public Sprite item4;
    public Sprite item5;

    [Header("Input Parameters")]
    public int mouseX;
    public int mouseY;

    // Privately managed inventory
    private Inventory inv;

    void Start () {
        inv = new Inventory(nWidth, nHeight);
        inv.AddItem(new GameItem(item1, item1, "item 1", "item 1", 50,
                    GameItem.ItemRarity.COMMON, 5, 64, 1));
        inv.AddItem(new GameItem(item2, item2, "item 2", "item 2", 40,
                    GameItem.ItemRarity.UNCOMMON, 4, 64, 2));
        inv.AddItem(new GameItem(item3, item3, "item 3", "item 3", 30,
                    GameItem.ItemRarity.RARE, 3, 64, 3));
        inv.AddItem(new GameItem(item4, item4, "item 4", "item 4", 20,
                    GameItem.ItemRarity.EPIC, 2, 64, 4));
        inv.AddItem(new GameItem(item5, item5, "item 5", "item 5", 10,
                    GameItem.ItemRarity.LEGENDARY, 1, 64, 5));
    }

    /// <summary>
    /// Draws the GUI on the screen
    /// </summary>
    void OnGUI()
    {
        if (visible)
        {
            // Center the inventory display
            int w = (nWidth + 1) * padding + nWidth * blockSize;
            int h = (nHeight + 1) * padding + nHeight * blockSize;
            int padx = (Screen.width - w) / 2;
            int pady = (Screen.height - h) / 2;

            // Inventory GUI background
            DrawQuad(new Rect(padx + offsetX, pady + offsetY, w, h),
                     background);
            
            for (int j = 0; j < nHeight; j++)
            {
                for (int i = 0; i < nWidth; i++)
                {
                    GameItem item = inv.GetItem(i, j);
                    int x = padx + offsetX + (i + 1) * padding + i * blockSize;
                    int y = pady + offsetY + (j + 1) * padding + j * blockSize;
                    Rect box = new Rect(x, y, blockSize, blockSize);

                    if (InBox(mouseX, mouseY, box))
                    {
                        // We are hovering over the item
                        DrawQuad(box, selected);
                    }
                    else
                    {
                        // Otherwise we want to show rarity
                        DrawQuad(box, RarityColor(item));
                    }
                    
                    if (item.SetYet())
                    {
                        GUI.Box(new Rect(x, y, blockSize, blockSize),
                                TextureFromSprite(item.GuiSprite));

                        // Show item name and quantity if we are hovering over
                        if (InBox(mouseX, mouseY, box))
                        {
                            GUI.Label(new Rect(x,
                                               y + blockSize - blockSize / 4,
                                               blockSize, blockSize / 4),
                                      item.Name + ": " + item.Quantity);
                        }
                    }
                }
            }
        }
    }

    /// <summary>
    /// Tests whether or not a given point is in a box
    /// </summary>
    /// <param name="x">x coordinate.</param>
    /// <param name="y">y coordinate.</param>
    /// <param name="r">the box.</param>
    private bool InBox(int x, int y, Rect r)
    {
        return x >= r.xMin && x <= r.xMax && y >= r.yMin && y <= r.yMax;
    }

    /// <summary>
    /// Returns the rarity color of the given item
    /// </summary>
    /// <param name="item">Item to examine.</param>
    private Color RarityColor(GameItem item)
    {
        switch (item.Rarity)
        {
            case GameItem.ItemRarity.COMMON:
                return colorCommon;
            case GameItem.ItemRarity.UNCOMMON:
                return colorUncommon;
            case GameItem.ItemRarity.RARE:
                return colorRare;
            case GameItem.ItemRarity.EPIC:
                return colorEpic;
            case GameItem.ItemRarity.LEGENDARY:
                return colorLegendary;
            default:
                return background;
        }
    }

    /// <summary>
    /// Draws a box with solid background
    /// </summary>
    /// <param name="r">The box.</param>
    /// <param name="c">The background color.</param>
    private void DrawQuad(Rect r, Color c)
    {
        Texture2D texture = new Texture2D(1, 1);
        GUIStyle gs = new GUIStyle();
        texture.SetPixel(0, 0, c);
        texture.Apply();
        gs.normal.background = texture;
        GUI.Box(r, texture, gs);
    }

    /// <summary>
    /// Yanks a texture representing the image of a sprite from the sprite
    /// </summary>
    /// <param name="s">The sprite.</param>
    private Texture2D TextureFromSprite(Sprite s)
    {
        if (s.rect.width != s.texture.width)
        {
            // Crop the texture
            Texture2D t = new Texture2D((int)s.rect.width, (int)s.rect.height);

            t.SetPixels(s.texture.GetPixels((int)s.textureRect.x,
                                            (int)s.textureRect.y,
                                            (int)s.textureRect.width,
                                            (int)s.textureRect.height));
            t.Apply();

            return t;
        }
        else
        {
            // We really don't need to do any processing here
            return s.texture;
        }
    }
}